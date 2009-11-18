import Char
import Control.Applicative
import Control.Monad
import Data.IORef
import Data.Ord
import Graphics.UI.Gtk
import qualified Graphics.UI.Gtk.ModelView as L
import Graphics.UI.Gtk.TreeList.TreeIter
import List
import Maybe

defaultSize :: Double
defaultSize = 24

compareFaces :: FontFace -> FontFace -> Ordering
compareFaces x y = case (show x,show y) of
                        ("Regular",_) -> LT
                        ("Book",_) -> LT
                        ("Roman",_) -> LT
                        (_,"Regular") -> GT
                        (_,"Book") -> GT
                        (_,"Roman") -> GT
                        (x',y') -> compare x' y'

treeIterIndex :: TreeIter -> Int
treeIterIndex (TreeIter _ i _ _) = fromEnum i

getSelectedIndex :: TreeView -> IO (Maybe Int)
getSelectedIndex view = do
    iter <- treeViewGetSelection view >>= treeSelectionGetSelected
    return $ treeIterIndex <$> iter

lower :: String -> String
lower = map toLower

scrollWidget :: (WidgetClass w) => w -> IO ScrolledWindow
scrollWidget w = do
    sw <- scrolledWindowNew Nothing Nothing
    set sw [scrolledWindowHscrollbarPolicy := PolicyAutomatic,
            scrolledWindowVscrollbarPolicy := PolicyAutomatic,
            scrolledWindowShadowType := ShadowIn]
    containerAdd sw w
    return sw

singleColumnTreeView :: L.ListStore a -> IO (TreeView,TreeViewColumn,CellRendererText)
singleColumnTreeView model = do
    view <- treeViewNew
    set view [treeViewModel := model, treeViewHeadersVisible := False]
    set view [treeViewFixedHeightMode := True]
    treeViewSetSearchColumn view 0

    selection <- treeViewGetSelection view
    set selection [treeSelectionMode := SelectionBrowse]

    column <- treeViewColumnNew
    cell <- cellRendererTextNew

    set column [treeViewColumnSizing := TreeViewColumnFixed,
                treeViewColumnFixedWidth := 300]

    set cell [L.cellTextEllipsize := EllipsizeMiddle,
              L.cellTextScale := 1.5]

    treeViewColumnPackStart column cell True
    treeViewAppendColumn view column
    return (view,column,cell)

fontList :: IO (L.ListStore FontFamily,TreeView)
fontList = do
    model <- L.listStoreNew []
    (view,column,cell) <- singleColumnTreeView model
    pango <- widgetCreatePangoContext view
    families <- sortBy (comparing $ lower . show) <$> contextListFamilies pango
    mapM_ (L.listStoreAppend model) families

    treeViewSetSearchEqualFunc view $ \col txt iter -> do
        family <- L.listStoreGetValue model $ treeIterIndex iter
        return $ lower txt `isPrefixOf` lower (show family)

    L.cellLayoutSetAttributes column cell model $
        \family -> [L.cellText := show family, L.cellTextFamily := show family]
    return (model,view)

faceList :: IO (L.ListStore FontFace,TreeView)
faceList = do
    model <- L.listStoreNew []
    (view,column,cell) <- singleColumnTreeView model

    treeViewSetSearchEqualFunc view $ \col txt iter -> do
        family <- L.listStoreGetValue model $ treeIterIndex iter
        return $ lower txt `isPrefixOf` lower (show family)

    L.cellLayoutSetAttributes column cell model $
        \style -> [L.cellText := show style,
            L.cellTextFontDesc :=> pangoFontFaceDescribe style]
    return (model,view)

previewNew :: IO TextView
previewNew = do
    view <- textViewNew
    buffer <- textViewGetBuffer view
    textBufferSetText buffer $ unlines ["AaBbCcDdEeFfG","gHhIiJjKkLlMm","NnOoPpQqRrSsT","tUuVvWwXxYyZz","0123456789","hamburgefontsiv","Pack my box with five dozen liquor jugs.","Cwm fjord bank glyphs vext quiz.","The quick brown fox jumps over the lazy dog."]
    textViewSetWrapMode view WrapWord
    textViewSetAcceptsTab view False
    return view

mainWindow :: IO Window
mainWindow = do
    w <- windowNew
    set w [windowTitle := "Font Viewer"]
    (model1,view1) <- fontList
    scroll1 <- scrollWidget view1
    set scroll1 [scrolledWindowHscrollbarPolicy := PolicyNever]
    (model2,view2) <- faceList
    scroll2 <- scrollWidget view2
    set scroll2 [scrolledWindowHscrollbarPolicy := PolicyNever]

    preview <- previewNew
    scroll3 <- scrollWidget preview

    selection1 <- treeViewGetSelection view1
    selection2 <- treeViewGetSelection view2

    sizeRef <- newIORef defaultSize
    slider <- vScaleNewWithRange 0 100 1
    set slider [scaleValuePos := PosLeft, rangeInverted := True, rangeValue := defaultSize]

    let updatePreview = do
        iter <- getSelectedIndex view2
        when (isJust iter) $ do
            face <- L.listStoreGetValue model2 $ fromJust iter
            desc <- pangoFontFaceDescribe face
            size <- readIORef sizeRef
            fontDescriptionSetSize desc size
            widgetModifyFont preview (Just desc)

    onRangeValueChanged slider $ do
        get slider rangeValue >>= writeIORef sizeRef
        updatePreview

    onSelectionChanged selection1 $ do
        iter <- getSelectedIndex view1
        when (isJust iter) $ do
            family <- L.listStoreGetValue model1 $ fromJust iter
            faces <- pangoFontFamilyListFaces family
            L.listStoreClear model2
            mapM_ (L.listStoreAppend model2) (sortBy compareFaces faces)
            Just faceIter <- treeModelGetIterFirst model2
            treeSelectionSelectIter selection2 faceIter

    onSelectionChanged selection2 updatePreview

    label1 <- labelNew (Just "Family:")
    label2 <- labelNew (Just "Style:")
    label3 <- labelNew (Just "Size:")
    label4 <- labelNew (Just "Preview:")
    mapM_ (`set` [miscXalign := 0]) [label1,label2,label3,label4]

    box1 <- vBoxNew False 6
    boxPackStart box1 label1 PackNatural 0
    boxPackStart box1 scroll1 PackGrow 0
    boxPackStart box1 label2 PackNatural 0
    boxPackStart box1 scroll2 PackGrow 0

    box2 <- vBoxNew False 6
    boxPackStart box2 label3 PackNatural 0
    boxPackStart box2 slider PackGrow 0

    box3 <- vBoxNew False 6
    boxPackStart box3 label4 PackNatural 0
    boxPackStart box3 scroll3 PackGrow 0

    box4 <- hBoxNew False 6
    boxPackStart box4 box1 PackNatural 0
    boxPackStart box4 box2 PackNatural 0
    boxPackStart box4 box3 PackGrow 0

    containerAdd w box4
    return w

main :: IO ()
main = do
    initGUI
    w <- mainWindow
    set w [windowDefaultWidth := 900,
           windowDefaultHeight := 650,
           containerBorderWidth := 12]
    onDestroy w mainQuit
    widgetShowAll w
    mainGUI
