$(document).ready(function(){
    var host = "ws://" + window.location.host + "/websocket";
    var server = new Server(host);
    var game = new BistrotanksGame(server);

    $("#joinGameBtn").on('click', function(e){
        var name = $('#name').val();
        if (/^([a-zA-Z0-9]{3,6})$/.test(name)) {
            var message = msgpack.encode({type: 'join_game', name: name});
            server.send(message);
        } else {
            alert('Format of name is invalid');
        }
        e.preventDefault();
    });
    $("#leaveGameBtn").on('click', function(e){
        server.send('leave_game');
        e.preventDefault();
    });
});
