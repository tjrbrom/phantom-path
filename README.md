# PhantomPath

A small WAI/Warp server that generates deterministic synthetic HTML pages and recursive links based on the request path and user-agent. Useful for controlled crawler tests or synthetic traffic experiments.

## Build

Run the following command to compile:

```bash
ghc -O2 PhantomPath.hs
```

## Run

After building, run:

```bash
./PhantomPath
```

The server listens on `0.0.0.0:8080`.

## Notes

- No storage or state; output depends only on path and user-agent.
- Known bots receive a simple static page.
- Other requests get generated content and recursive links.

## License

MIT
