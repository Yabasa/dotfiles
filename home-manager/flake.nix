{
  description = "My Home Manager flake";

  inputs =
    {
      nixpkgs.url = "github:nixos/nixpkgs/nixos-24.11";

      home-manager =
        {
          url = "github:nix-community/home-manager/release-24.11";
          inputs.nixpkgs.follows = "nixpkgs";
        };

      nixvim = {
        # If you are not running an unstable channel of nixpkgs, select the corresponding branch of nixvim.
        url = "github:nix-community/nixvim/nixos-24.11";
        inputs.nixpkgs.follows = "nixpkgs";
      };

      roc = {
        url = "github:roc-lang/roc";
        inputs.nixpkgs.follows = "nixpkgs";
      };

    };

  outputs =
    { nixpkgs
    , home-manager
    , nixvim
    , roc
    , ...
    }@inputs:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
      rocPkgs = roc.packages.${system};
    in
    {
      homeConfigurations = {
        "treb" = home-manager.lib.homeManagerConfiguration {
          inherit pkgs;
          modules = [
            ./home.nix
            nixvim.homeManagerModules.nixvim
          ];
          extraSpecialArgs = {
            rocPkgs = rocPkgs;
          };
        };
      };
    };
}
