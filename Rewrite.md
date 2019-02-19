# Rewrite

#Renamed Modules
[x] PixelEngine                     -> PixelEngine
[x] PixelEngine.Controls            -> PixelEngine
[x] PixelEngine.Graphics            -> PixelEngine
[x] PixelEngine.Graphics.Image      -> PixelEngine.Image
[x] PixelEngine.Graphics.Tile       -> PixelEngine.Tile
[x] PixelEngine.Graphics.Transition -> PixelEngine.Options
[x] PixelEngine.Graphics.Options    -> PixelEngine.Options
[x] PixelEngine.Location            -> Location
[x] PixelEngine.Grid                -> Grid
[x] PixelEngine.Grid.Position       -> Grid.Position
[x] PixelEngine.Grid.Bordered       -> Grid.Bordered
[x] PixelEngine.Grid.Direction      -> Grid.Direction

#Renamed Functions
[x] PixelEngine.Graphics.view               -> PixelEngine.toHtml
[x] PixelEngine.Graphics.Options.fromWidth  -> PixelEngine.Options.default
[x] PixelEngine.Graphics.Image.image        -> PixelEngine.Image.fromSrc
[x] PixelEngine.Graphics.Tile.tile          -> PixelEngine.Tile.fromPosition
[x] PixelEngine.Graphics.Transition.from    -> PixelEngine.Options.withTransitionFrom
[x] PixelEngine.Graphics.Transition.custom  -> PixelEngine.Graphics.transition
[x] PixelEngine.Controls.defaultLayout      -> PixelEngine.defaultInputs
[x] PixelEngine.Controls.supportingMobile   -> PixelEngine.Options.withMobileSupport

#Breaking Chances
[x] PixelEngine.Tile.animated               - now gets the number of sprites,
                                              not the number of steps Tile
[x] PixelEngine.Input                       - remove InputNone
[x] PixelEngine.toHtml                      - add "width" field to input
    PixelEngine.game                
    PixelEngine.gameWithControls
[x] PixelEngine.gameWithNoControls          - add "width" field to the return
                                              value of of the view
                                            - Options are now optional
                                            - controls are now (Input -> Maybe msg)
[x] PixelEngine.Options.default             - do not need a Float anymore.
[x] PixelEngine.Options.transition          - need:
                                              { start: String
                                              , keyFrames: List (Maybe String)
                                              , end: String
                                              }