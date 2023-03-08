# man-posframe.el 
View man pages on a popup frame using posframe

## Usage
```
(call-interactively 'man-posframe-show)
;; or
(man-posframe-show "3 printf")
```

## default keymap
```
    (define-key map (kbd "q") 'man-posframe-close)
    (define-key map (kbd "j") 'man-posframe-scroll-down)
    (define-key map (kbd "k") 'man-posframe-scroll-up)
    (define-key map (kbd "<escape>") 'man-posframe-close)
```
