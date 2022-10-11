{ pkgs, inputs, ... }:

{
  networking = {
    hostName = "weatherwax";
    computerName = "Weatherwax";
  };

  users = {
    knownUsers = [ "nick" ];

    users.nick = {
      home = "/Users/nick";
      description = "N. G. Scheurich";
      shell = pkgs.fish;
      uid = 501;
    };
  };
}
