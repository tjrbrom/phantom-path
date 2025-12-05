# PhantomPath

PhantomPath is a minimal WAI/Warp server that generates deterministic synthetic HTML pages.

- Each page includes a generated title, body, and a few recursive links.
- Output depends only on the request path and user-agent.
- Known search engine bots receive a simple static page.
- No storage or external state is used.

Useful for experiments, testing crawlers, or generating predictable synthetic pages.
