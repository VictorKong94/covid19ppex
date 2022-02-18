// Allows the "Enter" key to trigger authentication when in the password field
$(document).keyup(function(event) {
    if ($("#password").is(":focus") && (event.key == "Enter")) {
        $("#login").click();
    }
});

// Ends the user's session after x seconds of inactivity
var x = 180;
function idleTimer() {
    var t = setTimeout(logout, x * 1000);
    window.onmousemove = resetTimer; // catches mouse movements
    window.onmousedown = resetTimer; // catches mouse movements
    window.onclick = resetTimer;     // catches mouse clicks
    window.onscroll = resetTimer;    // catches scrolling
    window.onkeypress = resetTimer;  // catches keyboard actions
    
    function logout() {
        Shiny.setInputValue('timeout', true)
    }
    
    function resetTimer() {
        clearTimeout(t);
        t = setTimeout(logout, x * 1000);  // time in milliseconds
    }
}
idleTimer();
