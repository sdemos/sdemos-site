# sdemos.com

To build

```bash
stack install
sdemos-site build
```

To develop

```bash
sdemos-site watch
```

To deploy

```bash
git remote add deploy root@demos.zone:/var/repo/sdemos-site
git push deploy master    # staging.sdemos.com
git push deploy prod      # sdemos.com
git push deploy *         # dev.sdemos.com
```
