Bullet = function (bistrotanksGame, id, x, y, rotation) {
    this.bistrotanksGame = bistrotanksGame;
    this.id = id;
    this.bullet = this.bistrotanksGame.game.add.sprite(27, 9, 'bullet');
    this.bullet.anchor.setTo(0.5, 0.5);
    this.bullet.x = x;
    this.bullet.y = y;
    this.bullet.rotation = rotation;
};

Bullet.prototype.updatePosition = function(x, y) {
    this.bullet.x = x;
    this.bullet.y = y;
};

Bullet.prototype.die = function() {
    this.bullet.destroy();
};
