{
  description = "Incantations; or, A Set of Declarative System Configurations";

  nixConfig.bash-prompt = "\\033[32m\[nix-develop\:$PWD]$\\033[0m ";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";

    darwin.url = "github:LnL7/nix-darwin";
    darwin.inputs.nixpkgs.follows = "nixpkgs";

    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    flake-utils.url = "github:numtide/flake-utils";

    neovim-nightly.url = "github:nix-community/neovim-nightly-overlay";
  };

  outputs = { self, nixpkgs, darwin, home-manager, flake-utils, ... }@inputs:
    let
      inherit (flake-utils.lib) eachSystemMap;

      defaultSystems =
        [
          "aarch64-linux"
          "aarch64-darwin"
          "x86_64-darwin"
          "x86_64-linux"
        ];

      overlays = [ inputs.neovim-nightly.overlay ];

      mkDarwinSystem =
        { system ? "aarch64-darwin"
        , baseModules ? [
            home-manager.darwinModules.home-manager
            ./modules/darwin
          ]
        , extraModules ? [ ]
        }: darwin.lib.darwinSystem {
          inherit system;
          inputs = { inherit overlays; };
          modules = baseModules ++ extraModules;
        };

      mkNixosSystem =
        { system ? "x86_64-linux"
        , baseModules ? [
            home-manager.nixosModules.home-manager
            ./modules/nixos
          ]
        , extraModules ? [ ]
        }: nixpkgs.lib.nixosSystem {
          inherit system;
          inputs = { inherit overlays; };
          modules = baseModules ++ extraModules;
        };
    in
    {
      darwinConfigurations = {
        weatherwax = mkDarwinSystem {
          extraModules = [ ./hosts/weatherwax ];
        };
        ridcully = mkDarwinSystem {
          extraModules = [ ./hosts/ridcully ];
        };
      };

      nixosConfigurations = {
        twoflower = mkNixosSystem {
          extraModules = [ ./hosts/twoflower ];
        };
      };

      devShells = eachSystemMap defaultSystems (system:
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
        });
    };
}
