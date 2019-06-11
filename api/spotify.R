#install.packages("spotifyr")
library(httr)
library(rjson)
library(jsonlite)
library(purrr)


api_keys <- list("spotify"=list(client_id=tokens$spotify$username,
                                secret=tokens$spotify$password))

scope_access <- list(my_account="playlist-read-private",user="user-library-read")

##### 1. OAUTH TOKENS #####
authRef <- function(){
  clientID = api_keys$spotify$client_id 
  secret = api_keys$spotify$secret
  
  response = POST('https://accounts.spotify.com/api/token',
                  authenticate(clientID, secret,type="basic"),
                  body = list(grant_type = 'client_credentials',scope=scope_access$my_account),
                  encode = 'form', verbose(),
                  accept_json())
  
  token = content(response)$access_token 
  return(token)
}


##### 2. ARTIST API ####
getArtists<-function(artist_name,order=TRUE){
  token <- authRef()
  out <- NULL
  url <- paste0("https://api.spotify.com/v1/search?q=", gsub(" ", "%20", artist_name), "&type=artist")
  search <- content(GET(url, add_headers(Authorization=paste0("Bearer ",token)), accept_json()))
  artists <- length(search$artists$items)
  for(i in 1:artists){
    temp<-data.frame(
      artist_id = search$artists$items[[i]]$id,
      artist = search$artists$items[[i]]$name,
      url = search$artists$items[[i]]$external_urls$spotify,
      followers = search$artists$items[[i]]$followers$total,
      artist_pop = search$artists$items[[i]]$popularity,
      genre = toString(search$artists$items[[i]]$genres))
    rownames(temp)<-NULL
    out<-rbind(out, temp)
  }
  
  if(order==TRUE){
    out <- out[order(out$artist_pop,decreasing = T),]
  }
  
  return(out)
}

##### 3. ALBUMS FETCH #####
getAlbums<-function(artist_id){
  token <- authRef()
  out<-NULL
  offset <- 0
  limit <- 50
  album_url <-paste0("https://api.spotify.com/v1/artists/",as.character(artist_id),"/albums?offset=0&limit=1")
  album_search<-content(GET(album_url, add_headers(Authorization=paste0("Bearer ",token)), accept_json()))
  total_items <- as.numeric(album_search$total)
  while((limit+offset-1) <= total_items){
    album_url <-paste0("https://api.spotify.com/v1/artists/",as.character(artist_id),"/albums?offset=",offset,"&limit=",limit)
    album_search<-content(GET(album_url, add_headers(Authorization=paste0("Bearer ",token)), accept_json()))
    
    for(i in c(1:length(album_search$items))) {
      temp<-data.frame("id"= album_search$items[[i]]$id,
                       "name"= album_search$items[[i]]$name,
                       "created_at"=album_search$items[[i]]$release_date,
                       "url"= album_search$items[[i]]$external_urls,
                       "type"= album_search$items[[i]]$type)
      rownames(temp)<-NULL
      out<-rbind(out, temp)
    } 
    offset <- offset+51
    limit <- ifelse(total_items-limit<=50 & (total_items-limit > 0),total_items-limit,limit+50)
  }
  try(out <- out[!duplicated(out$name),])
  return(out)
}

#### 4. TRACK DATA ####
getTracks<-function(album_ids){
  token <- authRef()
  all_tracks <- NULL
  offset <- 0
  limit <- 50
  for(i in 1:length(album_ids)){
    track_url <-paste0("https://api.spotify.com/v1/albums/",album_ids[i], "/tracks?offset=0&limit=50")
    track_list <-content(GET(track_url,add_headers(Authorization=paste0("Bearer ",token)), accept_json()))
    for(j in 1:length(track_list$items)){
      temp<-data.frame(
        id=track_list$items[[j]]$id,
        album_id=album_ids[i],
        artist_id=track_list$items[[j]]$artists[[1]]$id,
        sno=track_list$items[[j]]$track_number,
        artist=track_list$items[[j]]$artists[[1]]$name,
        name=track_list$items[[j]]$name,
        duration=track_list$items[[j]]$duration_ms,
        url=track_list$items[[j]]$external_urls[[1]],
        type=track_list$items[[j]]$type,
        explicit=track_list$items[[j]]$explicit,
        preview_url=ifelse(is.null(track_list$items[[j]]$preview_url),'',track_list$items[[j]]$preview_url))
      
      all_tracks <-rbind(all_tracks, temp)
    }
  }  
  return(all_tracks)
}


#### 4. Audio Track Features ####

get_features <- function(track_id){
  token <- authRef()
  audio_url<-paste0("https://api.spotify.com/v1/audio-features/",track_id)
  audio_feature <- GET(audio_url, add_headers(Authorization=paste0("Bearer ",token)), accept_json())
  features <- as.data.frame(content(audio_feature))
  
  return(features)
}


get_analysis <- function(track_id){
  token <- authRef()
  audio_url<-paste0("https://api.spotify.com/v1/audio-analysis/",track_id)
  audio_feature <- GET(audio_url, add_headers(Authorization=paste0("Bearer ",token)), accept_json())
  audio_feature <- content(audio_feature)
  
  #segments <- audio_feature$segments
  track_features <- data.frame(audio_feature$track[c('num_samples', 'duration', 'sample_md5', 'offset_seconds', 'window_seconds', 'analysis_sample_rate', 'analysis_channels', 'end_of_fade_in', 'start_of_fade_out', 'loudness', 'tempo', 'tempo_confidence', 'time_signature', 'time_signature_confidence', 'key', 'key_confidence', 'mode', 'mode_confidence', 'code_version', 'echoprint_version', 'synch_version', 'rhythm_version')])
  beats <- do.call(rbind, lapply(audio_feature$beats, data.frame))
  bars <- do.call(rbind, lapply(audio_feature$bars, data.frame))
  sections <- do.call(rbind, lapply(audio_feature$sections, data.frame))
  
  
  return(list("track_features"=track_features,"beats"=beats,"bars"=bars,"sections"=sections))
}

raw <- readLines("https://files.pushshift.io/reddit/comments/sample_data.json")
map(audio_feature$beats[[1]], fromJSON)
xxx <- do.call(rbind,audio_feature$beats)
xxxx <- do.call(rbind, lapply(audio_feature$beats, data.frame)) 
#### 5. User API ####

get_users <- function(user_id){
  token <- authRef()
  user_url <-paste0("https://api.spotify.com/v1/users/",user_id)
  user_details <- content(GET(user_url, add_headers(Authorization=paste0("Bearer ",token)), accept_json()))
  user_data <- data.frame("username"=user_details$display_name,
                            "image_url"=user_details$images[[1]][["url"]],
                            "profile_url"=user_details$external_url$spotify,
                            "followers"=user_details$followers$total)
  
  return(user_data)
}
