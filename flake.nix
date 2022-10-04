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
      inherit (darwin.lib) darwinSystem;
      inherit (nixpkgs.lib) nixosSystem;
      overlays = [ inputs.neovim-nightly.overlay ];
    in
    {
      darwinConfigurations = {
        weatherwax = darwinSystem {
          system = "aarch64-darwin";
          inputs = { inherit overlays; };
          modules = [
            ./hosts/weatherwax
            home-manager.darwinModules.home-manager
          ];
        };

	ridcully = darwinSystem {
          system = "aarch64-darwin";
          inputs = { inherit overlays; };
          modules = [
            ./hosts/ridcully
            home-manager.darwinModules.home-manager
          ];
        };
      };

      nixosConfigurations = {
        twoflower = nixosSystem {
          system = "x86_64-linux";
          modules = [
            ./hosts/twoflower
            home-manager.nixosModules.home-manager
          ];
        };
      };
    };
}
