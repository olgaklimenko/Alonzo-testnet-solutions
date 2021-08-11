# How to build any plutus project

- Clone the `plutus` repo.
- Set up the binary caches following that doc https://github.com/input-output-hk/plutus#nix-1
- Check what the revision of plutus you need, go to `cabal.project` file inside your script project, find the plutus dependency and take the tag:

```
source-repository-package
  type: git
  location: https://github.com/input-output-hk/plutus
  tag: 826c2514a40e962c2e4d56ce912803a434cc28fe
```

In that example the tag is `826c2514a40e962c2e4d56ce912803a434cc28fe`.

- In the `plutus` project go to the commit with SHA equaled to that tag. And run `nix-shell`. 

- Wait while nix build the plutus project while nix shell will be enabled in you terminal.

- Go to your plutus script directory right from this shell (`mv <path-to-your-script>`)

- Run `cabal build` or `cabal run <executable name>`
