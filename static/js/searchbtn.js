$("#search-btn").click(function(event) {
    if(event) {
        event.preventDefault();
        window.location="/search/" + $("#search-field").val();
    }
});
