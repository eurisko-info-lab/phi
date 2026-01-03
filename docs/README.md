# Phi Documentation

This directory contains the Phi website, hosted via GitHub Pages.

## Structure

```
docs/
├── index.html          # Main landing page
├── playground/         # Interactive Phi playground
│   └── index.html
├── protocol/           # phi:// URL scheme documentation
│   └── index.html
└── README.md           # This file
```

## Development

To preview locally:

```bash
cd docs
python3 -m http.server 8000
# Open http://localhost:8000
```

## Deployment

The site is automatically deployed via GitHub Pages when pushed to main.

Configure in repo settings:
- Settings → Pages → Source: Deploy from branch
- Branch: main, folder: /docs

## phi:// Protocol

The `phi://` URL scheme enables direct linking to Phi specifications:

```
phi://eurisko-info-lab/phi/specs/phi-core/specs/phi.phi
phi://eurisko-info-lab/phi/specs/phi.phi:Cofree
phi://eurisko-info-lab/phi/specs/phi.phi#L42-L50
phi://registry/std/prelude.phi
```

See [protocol/](./protocol/) for full documentation.

## Links

- **Website**: https://eurisko-info-lab.github.io/phi/
- **Repository**: https://github.com/eurisko-info-lab/phi
- **Twitter**: [@euriskophi](https://twitter.com/euriskophi)
- **Bluesky**: [@phispec.bsky.social](https://bsky.app/profile/phispec.bsky.social)
