;;;; ============================================================================
;;;; AutoCAD 치수 스타일 LISP - 사용 예제
;;;; 파일명: example-usage.lsp
;;;; 설명: dimstyle.lsp 사용 예제 및 테스트 코드
;;;; ============================================================================

;;;; 예제 1: 기본 사용법
;;;; 명령어: MYDIM
;;;; 설명: DCL 대화상자를 통해 치수 스타일 생성
(defun example1 ()
  (princ "\n예제 1: 기본 사용법")
  (princ "\n명령어 입력: MYDIM")
  (princ "\nDCL 대화상자에서 옵션 조정 후 확인 클릭")
  (princ)
)

;;;; 예제 2: 프로그래밍 방식으로 치수 스타일 생성
;;;; 대화상자 없이 직접 값을 설정하여 치수 스타일 생성
(defun C:MAKEDIM (/ dimstyle-name)
  (princ "\n프로그래밍 방식으로 치수 스타일 생성...")
  
  (setq dimstyle-name "ISO-25-Auto")
  
  ;; 기존 스타일 삭제
  (if (tblsearch "DIMSTYLE" dimstyle-name)
    (command "._-DIMSTYLE" "_R" dimstyle-name "_Y")
  )
  
  ;; 새 스타일 생성
  (command "._-DIMSTYLE" "_S" dimstyle-name)
  
  ;; 설정 적용
  (setvar "DIMSCALE" 25)
  (setvar "DIMTXT" 2.5)
  (setvar "DIMGAP" 0.625)
  (setvar "DIMASZ" 2.5)
  (setvar "DIMEXO" 0.625)
  (setvar "DIMEXE" 1.25)
  (setvar "DIMTAD" 1)
  (setvar "DIMTIH" 0)
  (setvar "DIMTOH" 0)
  (setvar "DIMDLI" 10.0)
  (setvar "DIMUNIT" 2)
  (setvar "DIMDEC" 2)
  
  ;; 스타일 저장 및 적용
  (command "._-DIMSTYLE" "_S" dimstyle-name)
  (command "._-DIMSTYLE" "_R" dimstyle-name)
  
  (princ (strcat "\n치수 스타일 '" dimstyle-name "' 생성 완료!"))
  (princ)
)

;;;; 예제 3: 현재 치수 스타일 정보 출력
(defun C:DIMINFO (/)
  (princ "\n========================================")
  (princ "\n현재 치수 스타일 정보")
  (princ "\n========================================")
  (princ (strcat "\n현재 스타일: " (getvar "DIMSTYLE")))
  (princ (strcat "\n전체 축척 (DIMSCALE): " (rtos (getvar "DIMSCALE") 2 2)))
  (princ (strcat "\n문자 높이 (DIMTXT): " (rtos (getvar "DIMTXT") 2 2)))
  (princ (strcat "\n화살표 크기 (DIMASZ): " (rtos (getvar "DIMASZ") 2 2)))
  (princ (strcat "\n치수보조선 간격 (DIMEXO): " (rtos (getvar "DIMEXO") 2 2)))
  (princ (strcat "\n치수보조선 연장 (DIMEXE): " (rtos (getvar "DIMEXE") 2 2)))
  (princ (strcat "\n문자 간격 (DIMGAP): " (rtos (getvar "DIMGAP") 2 2)))
  (princ "\n========================================")
  (princ)
)

;;;; 예제 4: 여러 축척에 대한 치수 스타일 생성
(defun C:MAKEMULTIDIM (/)
  (princ "\n여러 축척의 치수 스타일 생성 중...")
  
  ;; 1:25 축척
  (make-dim-scale-style "ISO-25" 25 2.5)
  
  ;; 1:50 축척
  (make-dim-scale-style "ISO-50" 50 2.5)
  
  ;; 1:100 축척
  (make-dim-scale-style "ISO-100" 100 2.5)
  
  (princ "\n모든 축척 스타일 생성 완료!")
  (princ "\n사용 가능한 스타일: ISO-25, ISO-50, ISO-100")
  (princ)
)

;;;; 보조 함수: 축척별 치수 스타일 생성
(defun make-dim-scale-style (style-name scale text-height /)
  
  ;; 기존 스타일 삭제
  (if (tblsearch "DIMSTYLE" style-name)
    (command "._-DIMSTYLE" "_R" style-name "_Y")
  )
  
  ;; 새 스타일 생성
  (command "._-DIMSTYLE" "_S" style-name)
  
  ;; 축척 기반 설정
  (setvar "DIMSCALE" scale)
  (setvar "DIMTXT" text-height)
  (setvar "DIMGAP" (* text-height 0.25))
  (setvar "DIMASZ" text-height)
  (setvar "DIMEXO" (* text-height 0.25))
  (setvar "DIMEXE" (* text-height 0.5))
  (setvar "DIMTAD" 1)
  (setvar "DIMTIH" 0)
  (setvar "DIMTOH" 0)
  (setvar "DIMDLI" (* scale 0.4))
  (setvar "DIMUNIT" 2)
  (setvar "DIMDEC" 2)
  
  ;; 스타일 저장
  (command "._-DIMSTYLE" "_S" style-name)
  
  (princ (strcat "\n" style-name " (1:" (itoa scale) ") 생성됨"))
)

;;;; 예제 5: 치수 스타일 비교 테이블 출력
(defun C:DIMCOMPARE (/)
  (princ "\n========================================")
  (princ "\n치수 스타일 비교")
  (princ "\n========================================")
  (princ "\n스타일명       축척  문자높이  화살표")
  (princ "\n----------------------------------------")
  
  ;; 각 스타일 정보 출력 (스타일이 존재하는 경우)
  (if (tblsearch "DIMSTYLE" "ISO-25")
    (progn
      (command "._-DIMSTYLE" "_R" "ISO-25")
      (princ (strcat "\nISO-25         " 
                     (rtos (getvar "DIMSCALE") 2 0) 
                     "     " 
                     (rtos (getvar "DIMTXT") 2 1)
                     "      "
                     (rtos (getvar "DIMASZ") 2 1)))
    )
  )
  
  (if (tblsearch "DIMSTYLE" "ISO-50")
    (progn
      (command "._-DIMSTYLE" "_R" "ISO-50")
      (princ (strcat "\nISO-50         " 
                     (rtos (getvar "DIMSCALE") 2 0) 
                     "     " 
                     (rtos (getvar "DIMTXT") 2 1)
                     "      "
                     (rtos (getvar "DIMASZ") 2 1)))
    )
  )
  
  (if (tblsearch "DIMSTYLE" "ISO-100")
    (progn
      (command "._-DIMSTYLE" "_R" "ISO-100")
      (princ (strcat "\nISO-100        " 
                     (rtos (getvar "DIMSCALE") 2 0) 
                     "    " 
                     (rtos (getvar "DIMTXT") 2 1)
                     "      "
                     (rtos (getvar "DIMASZ") 2 1)))
    )
  )
  
  (princ "\n========================================")
  (princ)
)

;;;; 예제 6: 치수 그리기 헬퍼 명령어들
(defun C:DL () 
  (command "._DIMLINEAR") 
  (princ)
)

(defun C:DA () 
  (command "._DIMALIGNED") 
  (princ)
)

(defun C:DR () 
  (command "._DIMRADIUS") 
  (princ)
)

(defun C:DD () 
  (command "._DIMDIAMETER") 
  (princ)
)

(defun C:DAN () 
  (command "._DIMANGULAR") 
  (princ)
)

;;;; ============================================================================
;;;; 예제 로드 메시지
;;;; ============================================================================
(princ "\n========================================")
(princ "\n  치수 스타일 예제 파일 로드됨")
(princ "\n========================================")
(princ "\n  명령어:")
(princ "\n  MAKEDIM      - 자동 치수 스타일 생성")
(princ "\n  DIMINFO      - 현재 치수 정보 출력")
(princ "\n  MAKEMULTIDIM - 여러 축척 스타일 생성")
(princ "\n  DIMCOMPARE   - 치수 스타일 비교")
(princ "\n  ")
(princ "\n  빠른 치수 명령어:")
(princ "\n  DL  - 선형 치수")
(princ "\n  DA  - 정렬 치수")
(princ "\n  DR  - 반지름 치수")
(princ "\n  DD  - 직경 치수")
(princ "\n  DAN - 각도 치수")
(princ "\n========================================")
(princ)
