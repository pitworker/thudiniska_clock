;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                     ;;;
;;;                     Carnegie Mellon University                      ;;;
;;;                  and Alan W Black and Kevin Lenzo                   ;;;
;;;                      Copyright (c) 1998-2000                        ;;;
;;;                        All Rights Reserved.                         ;;;
;;;                                                                     ;;;
;;; Permission is hereby granted, free of charge, to use and distribute ;;;
;;; this software and its documentation without restriction, including  ;;;
;;; without limitation the rights to use, copy, modify, merge, publish, ;;;
;;; distribute, sublicense, and/or sell copies of this work, and to     ;;;
;;; permit persons to whom this work is furnished to do so, subject to  ;;;
;;; the following conditions:                                           ;;;
;;;  1. The code must retain the above copyright notice, this list of   ;;;
;;;     conditions and the following disclaimer.                        ;;;
;;;  2. Any modifications must be clearly marked as such.               ;;;
;;;  3. Original authors' names are not deleted.                        ;;;
;;;  4. The authors' names are not used to endorse or promote products  ;;;
;;;     derived from this software without specific prior written       ;;;
;;;     permission.                                                     ;;;
;;;                                                                     ;;;
;;; CARNEGIE MELLON UNIVERSITY AND THE CONTRIBUTORS TO THIS WORK        ;;;
;;; DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING     ;;;
;;; ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT  ;;;
;;; SHALL CARNEGIE MELLON UNIVERSITY NOR THE CONTRIBUTORS BE LIABLE     ;;;
;;; FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES   ;;;
;;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN  ;;;
;;; AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,         ;;;
;;; ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF      ;;;
;;; THIS SOFTWARE.                                                      ;;;
;;;                                                                     ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Phonset for cmu_thu
;;;

;;;  Feeel free to add new feature values, or new features to this
;;;  list to make it more appropriate to your language

;; This is where it'll fall over if you haven't defined a 
;; a phoneset yet, if you have, delete this, if you haven't
;; define one then delete this error message


(defPhoneSet
  cmu_thu
  ;;;  Phone Features
  (;; vowel or consonant
   (clst + - 0)
   (vc + - 0)
   ;; vowel length: short long dipthong schwa
   (vlng s l d a 0)
   ;; vowel height: high mid low
   (vheight 1 2 3 0 -)
   ;; vowel frontness: front mid back
   (vfront 1 2 3 0 -)
   ;; lip rounding
   (vrnd + - 0)
   ;; consonant type: stop fricative affricative nasal liquid approximant
   (ctype s f a n l r 0)
   ;; place of articulation: labial alveolar palatal
   ;; labio-dental dental velar glottal
   (cplace l a p b d v g 0)
   ;; consonant voicing
   (cvox + - 0)
   (asp  + - 0)
   (nuk + - 0)
   )
  (
   (    pau   -   -   0   0   0   0   0   0   -   -   -   ) 
   ( I   - + s 1 1 - 0 0 0 - - ) ;; ih
   ( II  - + l 1 1 - 0 0 0 - - ) ;; iy
   ( U   - + s 1 3 + 0 0 0 - - ) ;; uh
   ( UU  - + l 1 3 + 0 0 0 - - ) ;; uw
   ( E   - + s 2 1 - 0 0 0 - - ) ;; eh
   ( EE  - + d 2 1 - 0 0 0 - - ) ;; ey  ?
   ( O   - + l 3 3 + 0 0 0 - - ) ;; ao  ?
   ( OO  - + l 1 3 + 0 0 0 - - ) ;; uw  ?
   ( A   - + s 3 1 - 0 0 0 - - ) ;; ae  ?
   ( AA  - + l 3 3 - 0 0 0 - - ) ;; aa
   ( AU  - + d 3 2 - 0 0 0 - - ) ;; aw
   ( EI  - + d 2 1 - 0 0 0 - - ) ;; ey
   ( EU  - + s 2 1 - 0 0 0 - - ) ;; eh !FOLLOW WITH W
   ( IU  - + l 1 3 - 0 0 0 - - ) ;; uw !PRECEDE WITH J
   ( IA  - + d 1 1 - 0 0 0 - - ) ;; aa !PRECEDE WITH J
   ( OU  - + d 2 3 + 0 0 0 - - ) ;; ow
   ( UI  - + l 1 1 - 0 0 0 - - ) ;; iy !PRECEDE WITH W
   ( B   - - 0 0 0 0 s l - - - ) ;; p
   ( P   - - 0 0 0 0 s l - - - ) ;; p
   ( D   - - 0 0 0 0 s a - - - ) ;; t
   ( T   - - 0 0 0 0 s a - - - ) ;; t
   ( G   - - 0 0 0 0 s v - - - ) ;; k
   ( K   - - 0 0 0 0 s v - - - ) ;; k
   ( M   - - 0 0 0 0 n l + - - ) ;; m
   ( HM  - - 0 0 0 0 n l + - - ) ;; m
   ( N   - - 0 0 0 0 n a + - - ) ;; n
   ( HN  - - 0 0 0 0 n a + - - ) ;; n
   ( NG  - - 0 0 0 0 n v + - - ) ;; ng
   ( HNG - - 0 0 0 0 n v + - - ) ;; ng
   ( R   - - 0 0 0 0 r a + - - ) ;; r
   ( HR  - - 0 0 0 0 r a + - - ) ;; r
   ( F   - - 0 0 0 0 f b - - - ) ;; f
   ( V   - - 0 0 0 0 f b + - - ) ;; v
   ( TH  - - 0 0 0 0 f d - - - ) ;; th
   ( DH  - - 0 0 0 0 f d + - - ) ;; dh
   ( S   - - 0 0 0 0 f a - - - ) ;; s
   ( C   - - 0 0 0 0 f g - - - ) ;; hh
   ( Q   - - 0 0 0 0 f g + - - ) ;; hv
   ( H   - - 0 0 0 0 f g - - - ) ;; hh
   ( L   - - 0 0 0 0 l a + - - ) ;; l
   ( HL  - - 0 0 0 0 l a + - - ) ;; l
   ( J   - - 0 0 0 0 r p + - - ) ;; y
   ( W   - - 0 0 0 0 r l + - - ) ;; w
   ;; insert the phones here, see examples in 
   ;; festival/lib/*_phones.scm

   )
  )

(PhoneSet.silences '(pau))

(define (cmu_thu_scarpent::select_phoneset)
  "(cmu_thu_scarpent::select_phoneset)
Set up phone set for cmu_thu."
  (Parameter.set 'PhoneSet 'cmu_thu)
  (PhoneSet.select 'cmu_thu)
)

(define (cmu_thu_scarpent::reset_phoneset)
  "(cmu_thu_scarpent::reset_phoneset)
Reset phone set for cmu_thu."
  t
)

(provide 'cmu_thu_scarpent_phoneset)
