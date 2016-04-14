BistrotanksGame = function (server) {
    var self = this;
    this.userId = undefined;
    this.color = undefined;
    this.cursors = undefined;
    this.land = undefined;
    this.explosions = undefined;
    this.server = server;
    this.players = {};
    this.bullets = {};

    var phaserInit = function(){
        var keys = { 'up': Phaser.KeyCode.W, 'down': Phaser.KeyCode.S,
                     'left': Phaser.KeyCode.A, 'right': Phaser.KeyCode.D,
                     'fire': Phaser.KeyCode.SPACEBAR };
        self.cursors = self.game.input.keyboard.addKeys(keys);
        self.cursors.up.onDown.add(function(){ self.server.send('moving_up') });
        self.cursors.up.onUp.add(function(){ self.server.send('stop_moving_up') });
        self.cursors.down.onDown.add(function(){ self.server.send('moving_down') });
        self.cursors.down.onUp.add(function(){ self.server.send('stop_moving_down') });
        self.cursors.left.onDown.add(function(){ self.server.send('rotating_left') });
        self.cursors.left.onUp.add(function(){ self.server.send('stop_rotating_left') });
        self.cursors.right.onDown.add(function(){ self.server.send('rotating_right') });
        self.cursors.right.onUp.add(function(){ self.server.send('stop_rotating_right') });
        self.cursors.fire.onDown.add(function(){ self.server.send('fire') });
        self.game.input.keyboard.stop(); // initial state of keyboard
    };
    var preload = function(){
        console.log('preload finished');
        self.game.load.spritesheet('kaboom', 'images/explosion2.png', 32, 32, 23);
        self.game.load.atlas('tanks', 'images/tanks.png', 'images/tanks.json');
        self.game.load.image('grid', 'images/grid-2.png');
        self.game.load.image('bullet', 'images/bullet.png');
    };
    var create = function(){
        self.land = self.game.add.tileSprite(0, 0, 737, 545, 'grid');
        self.land.fixedToCamera = true;
        self.game.stage.backgroundColor = '#fff';

        //  Explosion pool
        self.explosions = self.game.add.group();
        for (var i = 0; i < 10; i++) {
            var explosionAnimation = self.explosions.create(0, 0, 'kaboom', [0], false);
            explosionAnimation.anchor.setTo(0.5, 0.5);
            explosionAnimation.animations.add('kaboom');
        }

        // game loaded, connect to server
        self.server.game = self;
        self.server.connect();
    };
    var update = function(){ };
    this.game = new Phaser.Game(737, 545, Phaser.AUTO, 'playground', { init: phaserInit, preload: preload,
                                                                       create: create, update: update });

};

BistrotanksGame.prototype.setInitialPlayers = function(rawPlayers) {
    for (var rawPlayer of rawPlayers) {
        this.players[rawPlayer['id']] = new Player(this, rawPlayer);
    }
    this.updateDashboard();
};

BistrotanksGame.prototype.enterTheGame = function() {
    this.game.input.keyboard.start();
};

BistrotanksGame.prototype.leaveTheGame = function() {
    this.game.input.keyboard.stop();
};

BistrotanksGame.prototype.playerDisconnected = function(data) {
    var player = this.players[data['id']];
    if (player) {
        if (data['id'] == this.userId) {
            this.leaveTheGame();
            $('.joinGame').show();
            $('.leaveGame').hide();
            $('#gameStatus').html('status: Spectator');
        }
        player.disconnect();
        delete this.players[data['id']];
        this.updateDashboard();
    }
};

BistrotanksGame.prototype.playerConnected = function(data) {
    if (this.players[data['id']]) { return }
    this.players[data['id']] = new Player(this, data);
    if (data['id'] == this.userId) {
        this.enterTheGame();
        $('.joinGame').hide();
        $('.leaveGame').show();
        $('#gameStatus').html('status: in game');
    }
    this.updateDashboard();
};

BistrotanksGame.prototype.playerMoved = function(data) {
    var player = this.players[data['id']];
    if (!player) { return }
    player.updatePosition(data['x'], data['y'], data['rotation']);
};

BistrotanksGame.prototype.playerGrewUp = function(data) {
    var player = this.players[data['id']];
    if (!player) { return }
    player.stopBlinking();
};

BistrotanksGame.prototype.killUser = function(data) {
    var playerKilled = this.players[data['killed_id']];
    if (!playerKilled) { return }
    playerKilled.die();
    var killer = 'unknown';
    if (data['killer_id'] != 'unknown') {
        killer = this.players[data['killer_id']];
        console.log(killer);
        killer.kills += 1;
    }
    this.updateDashboard();
    this.updateGameLog(playerKilled, killer);
};

BistrotanksGame.prototype.respawnPlayer = function(data) {
    var player = this.players[data['id']];
    if (!player) { return }
    player.respawn(data['x'], data['y'], data['rotation']);
};

BistrotanksGame.prototype.fireBullet = function(data) {
    this.bullets[data['id']] = new Bullet(this, data['id'], data['x'], data['y'], data['rotation']);
};

BistrotanksGame.prototype.updateBulletPosition = function(data) {
    var bullet = this.bullets[data['id']];
    if (!bullet) { return }
    bullet.updatePosition(data['x'], data['y']);
};

BistrotanksGame.prototype.killBullet = function(data) {
    var bullet = this.bullets[data['id']];
    if (!bullet) { return }
    bullet.die();
};

BistrotanksGame.prototype.updateDashboard = function() {
    var self = this;
    var sortedUuids = _.sortBy(Object.keys(this.players), function(uuid){ return self.players[uuid].kills; }).reverse();
    var container = "";
    for (var i = 0; i < sortedUuids.length; i++) {
        var player = this.players[sortedUuids[i]];
        container += "<tr id='" + player.userId + "'>";
        container += "<td>" + (i + 1) + "</td>";
        container += "<td><span class='name " + player.color + "'>" + player.name + "</span></td>";
        container += "<td>" + player.kills + "</td>";
        container += "<td>" + player.deaths + "</td>";
        container += "</tr>";
    }
    $("#scoreboard tbody").html(container);
};

BistrotanksGame.prototype.updateGameLog = function(playerKilled, killer) {
    var id = Date.now();
    var message = '<div class="item" id="' + id + '">';
    message += '<span class="name ' + playerKilled.color + '">' + playerKilled.name + '</span>';
    message += ' was killed by ';
    message += killer == 'unknown' ? '<em>unknown</em>' : '<span class="name ' + killer.color + '">' + killer.name + '</span>';
    $("#log").prepend(message);
    setTimeout(function(){ $('#' + id).remove() }, 20000);
};
