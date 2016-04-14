Player = function (game, rawData) {
    this.kills = rawData['kills'];
    this.deaths = rawData['deaths'];
    this.color = rawData['color'];
    this.name = rawData['name'];
    this.userId = rawData['id'];
    this.blinkTween = undefined;
    this.tankStatus = rawData['tank_status'];
    this.bistrotanksGame = game;
    this.game = game.game;
    this.tank = this.game.add.sprite(16, 16, 'tanks', rawData['color'] + '_tank_0');
    this.tank.anchor.setTo(0.5, 0.5);
    this.tank.x = rawData['x'];
    this.tank.y = rawData['y'];
    this.tank.rotation = rawData['rotation'];
    if (this.tankStatus == 'newborn') { this.startBlinking(); }
};

Player.prototype.updatePosition = function(x, y, rotation) {
    // TODO: add smooth animation
    this.tank.x = x;
    this.tank.y = y;
    this.tank.rotation = rotation;
};

Player.prototype.die = function() {
    if (this.userId == this.bistrotanksGame.userId) {
        this.bistrotanksGame.cursors.up.processKeyUp();
        this.bistrotanksGame.cursors.down.processKeyUp();
        this.bistrotanksGame.cursors.left.processKeyUp();
        this.bistrotanksGame.cursors.right.processKeyUp();
        this.bistrotanksGame.game.input.keyboard.stop();
    }
    this.deaths += 1;
    this.tank.kill();
    this.tankStatus = 'dead';
    var explosionAnimation = this.bistrotanksGame.explosions.getFirstExists(false, true);
    explosionAnimation.reset(this.tank.x, this.tank.y);
    explosionAnimation.play('kaboom', 30, false, true);
};

Player.prototype.respawn = function(x, y, rotation) {
    console.log('respawn');
    if (this.userId == this.bistrotanksGame.userId) {
        this.bistrotanksGame.game.input.keyboard.start();
    }
    this.tank.alpha = 1;
    this.tank.rotation = rotation;
    this.tank.reset(x, y);
    this.tankStatus = 'newborn';
    this.startBlinking();
};

Player.prototype.fire = function() {

};

Player.prototype.disconnect = function() {
    this.tank.destroy();
};

Player.prototype.startBlinking = function() {
    this.tank.alpha = 0;
    this.blinkTween = this.game.add.tween(this.tank).to( { alpha: 1 }, 250, "Linear", true, 0, -1);
    this.blinkTween.yoyo(true, 250);
};

Player.prototype.stopBlinking = function() {
    this.tankStatus = 'mature';
    if (this.blinkTween) {
        this.blinkTween.stop();
        this.tank.alpha = 1;
    }
};
