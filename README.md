# scenic

Yet another toy demo/graphics toolkit. Using CEPL.

Next iteration after, azimut/shiny and azimut/incandescent.

## Why?

- Clean the old repos.
- Stop relying of loosely related global variables.
- Have an easy way to define multiple lights/shadows.
- To support multiple "scenes".
- To pass the time.

## NOTE

it needs cepl/core/textures/texture.lisp/allocate-immutable-texture

``` common-lisp
(:texture-cube-map-array
 (tex-storage-3d texture-type (texture-mipmap-levels texture) (texture-image-format texture)
                 width height (* 6 (texture-layer-count texture))))
```

## License

MIT


Copyright (c) 2021 azimut <azimut.github@protonmail.com>


