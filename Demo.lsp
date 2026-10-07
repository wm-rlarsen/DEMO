;; DEMO TOOLS
;; Moves selected objects to suffixed demo layers and reverses them when needed.
;; Commands:
;;   DEMO       - Move selected objects to layers using the configured suffix.
;;   UNDEMO     - Move selected objects from suffixed layers back to existing base layers.
;;   DEMOCONFIG - Change suffix, color, no-plot setting, and optionally purge empty demo layers.
;;   DEMOPURGE  - Delete empty layers that end with the configured suffix.

(vl-load-com)

(defun demo:EnsureDefaults ()
  (if (not *WM-DEMO-SUFFIX*) (setq *WM-DEMO-SUFFIX* "-wmdemo"))
  (if (not *WM-DEMO-COLOR*) (setq *WM-DEMO-COLOR* "163"))
  (if (not (boundp '*WM-DEMO-NOPLOT*)) (setq *WM-DEMO-NOPLOT* T))
)

(defun demo:BoolText (val)
  (if val "Yes" "No")
)

(defun demo:LayerLockedP (layName / rec flags)
  (if (setq rec (tblsearch "LAYER" layName))
    (progn
      (setq flags (cdr (assoc 70 rec)))
      (= (logand flags 4) 4)
    )
    nil
  )
)

(defun demo:XrefLayerP (layName)
  (and layName (wcmatch layName "*|*"))
)

(defun demo:XrefObjectP (obj / path)
  (if (vlax-property-available-p obj 'Path)
    (progn
      (setq path (vl-catch-all-apply 'vla-get-Path (list obj)))
      (and (not (vl-catch-all-error-p path)) (/= path ""))
    )
    nil
  )
)

(defun demo:LayerObject (layers layName / result)
  (setq result (vl-catch-all-apply 'vla-item (list layers layName)))
  (if (vl-catch-all-error-p result) nil result)
)

(defun demo:EndsWith (str suffix / len slen)
  (setq len (strlen str)
        slen (strlen suffix))
  (and (> slen 0)
       (>= len slen)
       (= (strcase (substr str (1+ (- len slen)))) (strcase suffix)))
)

(defun demo:DemoLayerNameP (layName)
  (demo:EndsWith layName *WM-DEMO-SUFFIX*)
)

(defun demo:UsedLayerNames (/ ss i lay names)
  (setq names nil)
  (if (setq ss (ssget "_X"))
    (progn
      (setq i 0)
      (repeat (sslength ss)
        (setq lay (cdr (assoc 8 (entget (ssname ss i))))
              i (1+ i))
        (if lay
          (setq names (cons (strcase lay) names))
        )
      )
    )
  )
  names
)

(defun demo:CopyBaseLayerColor (layers baseLayName newLayer / baseLayer baseColor)
  (if (setq baseLayer (demo:LayerObject layers baseLayName))
    (progn
      (setq baseColor (vla-get-color baseLayer))
      (if (and baseColor (> baseColor 0))
        (vla-put-color newLayer baseColor)
      )
    )
  )
)

;; Creates a layer quickly. If color is "0", the new layer keeps the visual color of the base layer.
(defun demo:FastLayer (lyrs ln clr baseLayName / obj colorInt)
  (setq obj (vla-add lyrs ln))
  (setq colorInt (atoi clr))
  (cond
    ((and (> colorInt 0) (<= colorInt 255))
     (vla-put-color obj colorInt)
    )
    ((= colorInt 0)
     (demo:CopyBaseLayerColor lyrs baseLayName obj)
    )
  )
  (vla-put-plottable obj (if *WM-DEMO-NOPLOT* :vlax-false :vlax-true))
  obj
)

(defun demo:ObjectCanMoveP (ename layName / obj)
  (setq obj (vlax-ename->vla-object ename))
  (and layName
       (not (demo:LayerLockedP layName))
       (not (demo:XrefLayerP layName))
       (not (demo:XrefObjectP obj)))
)

(defun demo:SuffixStatus ()
  (strcat "Suffix: " *WM-DEMO-SUFFIX*
          " | Color: " (if (= (atoi *WM-DEMO-COLOR*) 0) "No change" *WM-DEMO-COLOR*)
          " | No-plot: " (demo:BoolText *WM-DEMO-NOPLOT*))
)

;; DEMO: MOVES SELECTED OBJECTS TO DEMO LAYER USING SUFFIX.
(defun c:DEMO (/ ss i ename layName demoLayName acDoc layers obj moved skipped err)
  (vl-load-com)
  (demo:EnsureDefaults)
  (setq moved 0 skipped 0)

  (princ (strcat "\nDEMO - Select objects to move to demo layers. (" (demo:SuffixStatus) ")"))
  (princ "\nLocked layers, xref layers, and viewports are skipped.")

  ;; Filter out Viewports immediately for speed/stability.
  (if (setq ss (ssget '((0 . "~VIEWPORT"))))
    (progn
      (setq acDoc  (vla-get-activedocument (vlax-get-acad-object))
            layers (vla-get-layers acDoc))
      (vla-startundomark acDoc)

      (repeat (setq i (sslength ss))
        (setq ename (ssname ss (setq i (1- i))))
        (setq layName (cdr (assoc 8 (entget ename))))

        (if (demo:ObjectCanMoveP ename layName)
          (progn
            (setq demoLayName (strcat layName *WM-DEMO-SUFFIX*))

            ;; Check/Create Layer. Color 0 preserves the source layer's visual color.
            (if (not (tblsearch "LAYER" demoLayName))
              (demo:FastLayer layers demoLayName *WM-DEMO-COLOR* layName)
            )

            ;; Apply current no-plot preference to the demo layer, including existing demo layers.
            (if (setq obj (demo:LayerObject layers demoLayName))
              (vla-put-plottable obj (if *WM-DEMO-NOPLOT* :vlax-false :vlax-true))
            )

            (setq err (vl-catch-all-apply 'vla-put-layer
                        (list (vlax-ename->vla-object ename) demoLayName)))
            (if (vl-catch-all-error-p err)
              (setq skipped (1+ skipped))
              (setq moved (1+ moved))
            )
          )
          (setq skipped (1+ skipped))
        )
      )
      (vla-endundomark acDoc)
      (princ (strcat "\nDEMO complete: " (itoa moved) " moved, " (itoa skipped) " skipped."))
    )
    (princ "\nNothing selected.")
  )
  (princ)
)

;; REVERSES THE DEMO FUNCTION FOR A GIVEN SUFFIX.
(defun c:UNDEMO (/ ss i ename layName suffixLen baseLayName acDoc moved skipped err)
  (vl-load-com)
  (demo:EnsureDefaults)
  (setq moved 0 skipped 0)

  (princ (strcat "\nUNDEMO - Select objects on layers ending with '" *WM-DEMO-SUFFIX* "'."))
  (princ "\nBase layers must already exist. Run DEMOCONFIG to change the suffix.")

  (if (setq ss (ssget '((0 . "~VIEWPORT"))))
    (progn
      (setq acDoc (vla-get-activedocument (vlax-get-acad-object)))
      (vla-startundomark acDoc)
      (setq suffixLen (strlen *WM-DEMO-SUFFIX*)
            i 0)

      (repeat (sslength ss)
        (setq ename (ssname ss i)
              layName (cdr (assoc 8 (entget ename)))
              i (1+ i))

        (if (and layName
                 (demo:DemoLayerNameP layName)
                 (demo:ObjectCanMoveP ename layName))
          (progn
            (setq baseLayName (substr layName 1 (- (strlen layName) suffixLen)))
            (if (tblsearch "LAYER" baseLayName)
              (progn
                (setq err (vl-catch-all-apply 'vla-put-layer
                            (list (vlax-ename->vla-object ename) baseLayName)))
                (if (vl-catch-all-error-p err)
                  (setq skipped (1+ skipped))
                  (setq moved (1+ moved))
                )
              )
              (progn
                (setq skipped (1+ skipped))
                (princ (strcat "\nSkipped: Base layer '" baseLayName "' does not exist."))
              )
            )
          )
          (setq skipped (1+ skipped))
        )
      )

      (vla-endundomark acDoc)
      (princ (strcat "\nUNDEMO complete: " (itoa moved) " moved, " (itoa skipped) " skipped."))
    )
    (princ "\nNothing selected.")
  )
  (princ)
)

(defun demo:PurgeEmptyDemoLayers (/ acDoc layers layerList layObj layName currentLayer usedLayers deleted skipped err)
  (vl-load-com)
  (demo:EnsureDefaults)
  (setq acDoc (vla-get-activedocument (vlax-get-acad-object))
        layers (vla-get-layers acDoc)
        currentLayer (getvar "CLAYER")
        usedLayers (demo:UsedLayerNames)
        layerList nil
        deleted 0
        skipped 0)

  ;; Build a list first so deleting layers does not disturb collection iteration.
  (vlax-for layObj layers
    (setq layName (vla-get-name layObj))
    (if (and (demo:DemoLayerNameP layName)
             (/= (strcase layName) (strcase currentLayer))
             (not (demo:LayerLockedP layName))
             (not (demo:XrefLayerP layName))
             (not (member (strcase layName) usedLayers)))
      (setq layerList (cons layName layerList))
    )
  )

  (foreach layName layerList
    (setq layObj (demo:LayerObject layers layName))
    (if layObj
      (progn
        (setq err (vl-catch-all-apply 'vla-delete (list layObj)))
        (if (vl-catch-all-error-p err)
          (setq skipped (1+ skipped))
          (setq deleted (1+ deleted))
        )
      )
    )
  )

  (princ (strcat "\nDEMOPURGE complete: " (itoa deleted) " empty demo layers deleted, " (itoa skipped) " skipped."))
  (princ)
)

(defun c:DEMOPURGE ()
  (demo:PurgeEmptyDemoLayers)
)

;; CONFIGURES VARIABLES FOR DEMO AND UNDEMO.
(defun demo:PromptSuffix (/ tmp)
  (setq tmp (getstring T (strcat "\nEnter layer suffix <" *WM-DEMO-SUFFIX* ">: ")))
  (if (and tmp (/= tmp ""))
    (setq *WM-DEMO-SUFFIX* tmp)
  )
)

(defun demo:PromptColor (/ tmp val)
  ;; Use getstring instead of getint so a typed 0 is always captured and stored.
  (setq tmp (getstring T (strcat "\nEnter layer color (0=keep source layer color, 1-255=ACI color) <" *WM-DEMO-COLOR* ">: ")))
  (cond
    ((= tmp "") nil)
    ((wcmatch tmp "#*")
     (setq val (atoi tmp))
     (if (and (>= val 0) (<= val 255) (= tmp (itoa val)))
       (setq *WM-DEMO-COLOR* (itoa val))
       (princ "\nColor not changed. Enter a whole number from 0 to 255.")
     )
    )
    (t
     (princ "\nColor not changed. Enter a whole number from 0 to 255.")
    )
  )
)

(defun demo:PromptNoPlot (/ kw)
  (initget "Yes No")
  (setq kw (getkword (strcat "\nSet demo layers to no-plot? [Yes/No] <" (demo:BoolText *WM-DEMO-NOPLOT*) ">: ")))
  (cond
    ((= kw "Yes") (setq *WM-DEMO-NOPLOT* T))
    ((= kw "No")  (setq *WM-DEMO-NOPLOT* nil))
  )
)

(defun demo:PromptPurge (/ kw)
  (initget "Yes No")
  (setq kw (getkword (strcat "\nPurge empty layers ending with '" *WM-DEMO-SUFFIX* "' now? [Yes/No] <No>: ")))
  (if (= kw "Yes")
    (demo:PurgeEmptyDemoLayers)
  )
)

(defun c:DEMOCONFIG (/ kw done)
  (vl-load-com)
  (demo:EnsureDefaults)
  (setq done nil)

  (princ "\n--- DEMO Configuration ---")
  (while (not done)
    (princ (strcat "\nCurrent settings: " (demo:SuffixStatus)))
    (initget "Suffix Color Noplot Purge All Exit")
    (setq kw (getkword "\nChange [Suffix/Color/Noplot/Purge/All/Exit] <Exit>: "))
    (cond
      ((or (null kw) (= kw "Exit"))
       (setq done T)
      )
      ((= kw "Suffix")
       (demo:PromptSuffix)
      )
      ((= kw "Color")
       (demo:PromptColor)
      )
      ((= kw "Noplot")
       (demo:PromptNoPlot)
      )
      ((= kw "Purge")
       (demo:PromptPurge)
      )
      ((= kw "All")
       (demo:PromptSuffix)
       (demo:PromptColor)
       (demo:PromptNoPlot)
       (demo:PromptPurge)
       (setq done T)
      )
    )
  )

  (princ (strcat "\nSettings updated: " (demo:SuffixStatus)))
  (princ)
)

(princ "\nDEMO tools loaded. Commands: DEMO, UNDEMO, DEMOCONFIG, DEMOPURGE.")
(princ "\nTip: Use DEMOCONFIG to set suffix, color, no-plot behavior, or purge empty suffixed layers.")
(princ)
