(defconstant *ampercent*
  '("& "
     . "&amp; "))

;; (defconstant *close-punct-re*
;;   '("([)]}༻༽᚜⁆⁾₎〉❩❫❭❯❱❳❵⟆⟧⟩⟫⟭⟯⦄⦆⦈⦊⦌⦎⦐⦒⦔⦖⦘⧙⧛⧽⸣⸥⸧⸩〉》」』】〕〗〙〛〞〟﴿︘︶︸︺︼︾﹀﹂﹄﹈﹚﹜﹞）］｝｠｣])"
;;      . "\\1 "))

(defconstant *comma-in-num*
  '("(?<!,)([,،])(?![,\\d])"
     . " \\1 "))

;; (defconstant *currency-sym-re*
;;   '("([$¢£¤¥֏؋৲৳৻૱௹฿៛₠₡₢₣₤₥₦₧₨₩₪₫€₭₮₯₰₱₲₳₴₵₶₷₸₹₺꠸﷼﹩＄￠￡￥￦])"
;;      . "\\1 "))

(defconstant *en-em-dashes*
  '("([–—])"
     . " \\1 "))

(defconstant *final-period-1*
  '("(?<![.])[.]$"
     . " ."))

;; (defconstant *final-period-2*
;;   '("(?<![.])[.]\s*([\\"'’»›”]) *$"
;;      . " . \\1"))

(defconstant *lstrip*
  '("^ +"
     . ""))

(defconstant *multi-commas*
  '("(,{2,})"
     . " \\1 "))

(defconstant *multi-dashes*
  '("(-{2,})"
     . " \\1 "))

(defconstant *multi-dots*
  '("([.]{2,})"
     . " \\1 "))

(defconstant *non-breaking*
  '(" "
     . " "))

(defconstant *one-space*
  '(" {2,}"
     . " "))

;; (defconstant *open-punct-re*
;;   '("([([{༺༼᚛‚„⁅⁽₍〈❨❪❬❮❰❲❴⟅⟦⟨⟪⟬⟮⦃⦅⦇⦉⦋⦍⦏⦑⦓⦕⦗⧘⧚⧼⸢⸤⸦⸨〈《「『【〔〖〘〚〝﴾︗︵︷︹︻︽︿﹁﹃﹇﹙﹛﹝（［｛｟｢])"
;;      . "\\1 "))

(defconstant *pipe*
  '("[|]"
     . " &#124; "))

(defconstant *prob-single-quotes*
  '("(['’`])"
     . " \\1 "))

(defconstant *rstrip*
  '("\s+$"
     . "
"))

(defconstant *stupid-quotes-1*
  '(" ` ` "
     . " `` "))

(defconstant *stupid-quotes-2*
  '(" ' ' "
     . " '' "))

(defconstant *tab*
  '("	"
     . " &#9; "))

(defconstant *url-foe-1*
  '(":(?!//)"
     . " : "))

(defconstant *url-foe-2*
  '("\\?(?!\S)"
     . " ? "))

(defconstant *url-foe-3*
  '("(:\/\/)[\S+[.]\S+\/\S+][\/]"
     . " / "))

(defconstant *url-foe-4*
  '(" /"
     . " / "))

(defconstant *toktok-regex-pairs*
  (list
        *non-breaking*
        ;; *funky-punct-1*
        ;; *funky-punct-2*
        *url-foe-1*
        *url-foe-2*
        *url-foe-3*
        *url-foe-4*
        *ampercent*
        *tab*
        *pipe*
        ;; *open-punct-re*
        ;; *close-punct-re*
        *multi-commas*
        *comma-in-num*
        *prob-single-quotes*
        *stupid-quotes-1*
        *stupid-quotes-2*
        ;; *currency-sym-re*
        *en-em-dashes*
        *multi-dashes*
        *multi-dots*
        *final-period-1*
        ;; *final-period-2*
        *one-space*
        ))
