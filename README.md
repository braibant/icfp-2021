Solution to ICFPC 2021
======================

![Screenshot](icfp2021.png "Screenshot")

## Intro

The main tool is `viz.exe` ([`app/viz/viz.ml`](app/viz/viz.ml)).
Fundamentally, it's a UI to move nodes around and save poses, but it
has lots of features to help with the mechanical work.

## Build & run

```
$ dune external-lib-deps --missing @all
# install any missing deps listed above
$ dune build @all
$ _build/default/app/viz/viz.exe display problems/example1.json
```

## Team

- Dmitry Astapov (`dastapov AT gmail DOT com`)
- Thomas Braibant (`thomas DOT braibant AT gmail DOT com`)
- Alexandru Scvorţov (`icfp2021 AT scvalex DOT net`)

## Timeline

- Initially implemented a GUI, with manual placing of vertices (day 1)
- Added an automated solver maximizing XXXX (day 2) 
- Added some rudimentary physics engine to help coalesce edges (day 2). When
  performance became an issue moved from arbitrary precision based vectors to
  float based vectors (doh).
- Made vertex movement use the physics engine to move vertices more efficiently (day 2)
- Added some "untangling" mode to the physics engine (day 3)
- Added some micro-optimizer to help fine tune our solutions (day 3)


## References

* https://icfpcontest2021.github.io/index.html
* https://icfpcontest2021.github.io/faq.html
* https://poses.live/problems
