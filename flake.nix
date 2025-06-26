{
  description = "Incantations; or, A Set of Declarative System Configurations";

  nixConfig.bash-prompt = "[nix-develop\:$PWD]$ ";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";

    darwin.url = "github:LnL7/nix-darwin";
    darwin.inputs.nixpkgs.follows = "nixpkgs";

    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      darwin,
      home-manager,
      ...
    }@inputs:
    let
      inherit (inputs.flake-utils.lib) eachSystemMap;

      defaultSystems = [
        "aarch64-darwin"
        "x86_64-linux"
      ];

      mkDarwinSystem =
        {
          system ? "aarch64-darwin",
          baseModules ? [
            home-manager.darwinModules.home-manager
            ./modules/darwin
          ],
          extraModules ? [ ],
        }:
        darwin.lib.darwinSystem {
          inherit system;
          inherit inputs;
          modules = baseModules ++ extraModules;
        };
    in
    {
      darwinConfigurations = {
        bienjensu = mkDarwinSystem {
          extraModules = [ ./hosts/bienjensu ];
        };
        joulie = mkDarwinSystem {
          extraModules = [ ./hosts/joulie ];
        };
      };

      devShells = eachSystemMap defaultSystems (
        system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
        in
        {
          default = pkgs.mkShell {
            nativeBuildInputs = with pkgs; [
              rustc
              cargo
              rust-analyzer
            ];
          };
        }
      );
    };
}
