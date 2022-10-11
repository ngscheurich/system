# ----------------------------------------------------------
# Packages for working with cloud computing services
# ----------------------------------------------------------

{ pkgs, ... }:

{
  home.packages = with pkgs; [
    aws-vault
    awscli2
    nodePackages.serverless
    ssm-session-manager-plugin
    terraform
  ];
}
