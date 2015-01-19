action social_networks(search_terms: String){
    $messages := social→search("twitter", $search_terms);
}