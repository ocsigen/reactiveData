* To update dev documentation
```
make gh-pages
```

* To release
```
make release VERSION=1.0.0
git push && git push --tags
make prepare VERSION=1.0.0
```
Then check that everything is fine and follow the instructions.
Don't forget to change the doc field inside the opam file.
