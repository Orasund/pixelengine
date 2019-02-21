# Rewrite

# Renamed Modules

```
PixelEngine                             -> PixelEngine
PixelEngine.Controls                    -> PixelEngine
PixelEngine.Graphics                    -> PixelEngine
PixelEngine.Graphics.Image              -> PixelEngine.Image
PixelEngine.Graphics.Tile               -> PixelEngine.Tile
PixelEngine.Graphics.Transition         -> PixelEngine.Options
PixelEngine.Graphics.Options            -> PixelEngine.Options
PixelEngine.Location                    -> Location
PixelEngine.Grid                        -> Grid
PixelEngine.Grid.Position               -> Grid.Position
PixelEngine.Grid.Bordered               -> Grid.Bordered
PixelEngine.Grid.Direction              -> Grid.Direction
```

# Renamed Functions

```
PixelEngine.Graphics.view               -> PixelEngine.toHtml
PixelEngine.Graphics.Options.fromWidth  -> PixelEngine.Options.default
PixelEngine.Graphics.Image.image        -> PixelEngine.Image.fromSrc
PixelEngine.Graphics.Tile.tile          -> PixelEngine.Tile.fromPosition
PixelEngine.Graphics.Transition.from    -> PixelEngine.Options.withTransitionFrom
PixelEngine.Graphics.Transition.custom  -> PixelEngine.Graphics.transition
PixelEngine.Controls.defaultLayout      -> PixelEngine.defaultInputs
PixelEngine.Controls.supportingMobile   -> PixelEngine.Options.withMobileSupport
```

# Breaking Chances

```
PixelEngine.Tile.animated               - now gets the number of sprites,
                                          not the number of steps Tile
PixelEngine.Input                       - remove InputNone
PixelEngine.toHtml                      - add "width" field to input
PixelEngine.game                
PixelEngine.gameWithControls
PixelEngine.gameWithNoControls          - add "width" field to the return
                                          value of of the view
                                        - Options are now optional
                                        - controls are now (Input -> Maybe msg)
PixelEngine.Options.default             - do not need a Float anymore.
PixelEngine.Options.transition          - need:
                                            { start: String
                                            , keyFrames: List (Maybe String)
                                            , end: String
                                            }
```