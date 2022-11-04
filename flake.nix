{
  description = "Incantations; or, A Set of Declarative System Configurations";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";

    darwin.url = "github:LnL7/nix-darwin";
    darwin.inputs.nixpkgs.follows = "nixpkgs";

    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    neovim-nightly.url = "github:nix-community/neovim-nightly-overlay";
  };

  outputs = { self, nixpkgs, darwin, home-manager, ... }@inputs:
    let
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
    };
}
