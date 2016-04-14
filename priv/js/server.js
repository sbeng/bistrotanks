Server = function (host) {
    this.pingInterval = undefined;
    this.pingRequestSentAt = undefined;
    this.game = undefined;
    this.host = host;
    this.socket = undefined;
};

Server.prototype.connect = function() {
    if (!("WebSocket" in window)) {
        alert("websockets are not supported");
        return;
    }
    var self = this;
    this.writeToLog("Connecting..");
    this.socket = new WebSocket(this.host);
    this.socket.binaryType = "arraybuffer";
    this.socket.onopen = function() {
        $('#connectionStatus').html('Welcome to Bistrotanks!');
        $('#gameStatus').html('status: Spectator');
        $('.joinGame').show();
        self.pingInterval = setInterval(function(){
            self.pingRequestSentAt = (new Date()).getTime(); self.socket.send("ping");
        }, 1500);
    };
    this.socket.onclose = function(event) {
        var message;
        if (event.wasClean) {
            message = "Disconnected from server.";
            self.writeToLog(message);
        } else {
            message = "Server terminated connection.";
            self.writeToLog(message);
        }
        self.connectionClosed(message);
    };

    this.socket.onmessage = function(event) {
        if (event.data == 'pong') { self.calculateLatency((new Date()).getTime()); return; }
        var raw_binary_data = new Uint8Array(event.data);
        var message = msgpack.decode(raw_binary_data);
        var data = message['data'];
        switch(message['type']) {
            case 'registered':
                self.game.setInitialPlayers(data['players']);
                self.game.userId = data['userId'];
                self.game.color = data['color'];
                break;
            case 'world_updates':
                if (self.cacheEnabled) {
                    self.cache.push(message['data']);
                    break;
                }
                self.updateTheWorld(message['data']);
                break;
            case 'failed_to_join':
                alert(message['reason']);
                break;
        }
    };

    this.socket.onerror = function(error) {
        self.connectionClosed("Something wrong with connection to server.");
        clearInterval(self.pingInterval);
        console.log('error with socket: ' + error);
    };
};

Server.prototype.connectionClosed = function(message) {
    clearInterval(this.pingInterval);
    $('.joinGame, .leaveGame').hide();
    $('#connectionStatus').html('Not connected.');
    $('#ping, #players-count, #spectators-count').html('unknown');
    $('#gameStatus').html('status: unknown');
    $('#serverModal .modal-body p').html(message);
    $('#serverModal').modal({keyboard: false, backdrop: 'static'});
};

Server.prototype.writeToLog = function(message) {
    var eventData = moment().format("DD/MM/YYYY HH:mm:ss.SS");
    var gameConsole = $("#console")[0];
    //var needToScroll = gameConsole.scrollTop == gameConsole.scrollHeight - 149; // height of block is 150px
    $(gameConsole).append("<div>[" + eventData + "] " + message + "</div>");
    //if (needToScroll) {
        gameConsole.scrollTop = gameConsole.scrollHeight;
    //}
};

Server.prototype.calculateLatency = function(pingResponseReceivedAt) {
    var latency = pingResponseReceivedAt - this.pingRequestSentAt;
    $("#ping").html(latency + 'ms');
};

Server.prototype.updateTheWorld = function(events) {
    for (var event of events) {
        var data = event['data'];
        switch(event['type']) {
            case 'connected':
                this.writeToLog('Spectator connected: ' + data['ip']);
                break;
            case 'disconnected':
                this.writeToLog('Spectator disconnected: ' + data['ip']);
                this.game.playerDisconnected(data);
                break;
            case 'number_of_users':
                $("#players-count").html(data['players']);
                $("#spectators-count").html(data['spectators']);
                break;
            case 'joined_game':
                this.writeToLog("Player " + '<span class="name ' + data['color'] + '">' + data['name'] + "</span> joined the game!");
                this.game.playerConnected(data);
                break;
            case 'left_game':
                this.writeToLog("Player " + '<span class="name ' + data['color'] + '">' + data['name'] + "</span> left the game!");
                this.game.playerDisconnected(data);
                break;
            case 'updated_position':
                this.game.playerMoved(data);
                break;
            case 'tank_grew_up':
                this.game.playerGrewUp(data);
                break;
            case 'user_killed':
                this.game.killUser(data);
                break;
            case 'respawn':
                this.game.respawnPlayer(data);
                break;
            case 'bullet_fired':
                this.game.fireBullet(data);
                break;
            case 'bullet_position':
                this.game.updateBulletPosition(data);
                break;
            case 'bullet_dead':
                this.game.killBullet(data);
                break;
        }
    }

};

Server.prototype.send = function(message) { this.socket.send(message); };

// move to right place
Server.prototype.capitalize = function(word) {
    return word.charAt(0).toUpperCase() + word.slice(1);
};
