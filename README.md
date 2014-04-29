# Phantom

Phantom is a tiny password manager which doesn't store any passwords
to disk. It just creates new ones from the output of a PRNG seeded
with your master key.

Your account names are stored in

    ~/.phantoms

by default. If you want to change the path,
edit the [Config.hs](./src/Phantom/Config.hs) file.

### License

Phantom is released under MIT license.
You can find a copy of the MIT License in the [LICENSE](./LICENSE) file.
