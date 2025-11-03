;;;; ============================================================================
;;;; AutoCAD 치수 스타일 생성 LISP 프로그램
;;;; 파일명: dimstyle.lsp
;;;; 설명: ISO-25 기반 치수 스타일을 생성하고 DCL 대화상자로 옵션 조정
;;;; ============================================================================

(defun C:MYDIM (/ dcl_id result dimstyle-name)
  (princ "\n치수 스타일 생성 프로그램 시작...")
  
  ;; DCL 파일 로드
  (setq dcl_id (load_dialog "dimstyle.dcl"))
  
  (if (not (new_dialog "dimstyle" dcl_id))
    (progn
      (alert "DCL 대화상자를 로드할 수 없습니다!")
      (exit)
    )
  )
  
  ;; 기본값 설정
  (setq *dim_scale* "25")          ; 치수 전체 축척
  (setq *dim_text_height* "2.5")   ; 문자 높이
  (setq *dim_arrow_size* "2.5")    ; 화살표 크기
  (setq *dim_ext_offset* "0.625")  ; 치수보조선 간격띄우기
  (setq *dim_ext_extend* "1.25")   ; 치수보조선 연장
  (setq *dim_text_gap* "0.625")    ; 문자와 치수선 간격
  
  ;; DCL 컨트롤 초기화
  (set_tile "dimscale" *dim_scale*)
  (set_tile "textheight" *dim_text_height*)
  (set_tile "arrowsize" *dim_arrow_size*)
  (set_tile "extoffset" *dim_ext_offset*)
  (set_tile "extextend" *dim_ext_extend*)
  (set_tile "textgap" *dim_text_gap*)
  
  ;; 액션 설정
  (action_tile "dimscale" "(setq *dim_scale* $value)")
  (action_tile "textheight" "(setq *dim_text_height* $value)")
  (action_tile "arrowsize" "(setq *dim_arrow_size* $value)")
  (action_tile "extoffset" "(setq *dim_ext_offset* $value)")
  (action_tile "extextend" "(setq *dim_ext_extend* $value)")
  (action_tile "textgap" "(setq *dim_text_gap* $value)")
  
  (action_tile "accept" "(done_dialog 1)")
  (action_tile "cancel" "(done_dialog 0)")
  
  ;; 대화상자 표시
  (setq result (start_dialog))
  
  (unload_dialog dcl_id)
  
  ;; 사용자가 확인을 누른 경우
  (if (= result 1)
    (progn
      (princ "\n치수 스타일 생성 중...")
      
      ;; 치수 스타일 생성
      (setq dimstyle-name "ISO-25-Custom")
      (create_dimstyle dimstyle-name)
      
      ;; 치수 그리기 시작
      (princ "\n치수를 그리려면 DIM 명령어를 사용하세요.")
      (princ (strcat "\n현재 치수 스타일: " dimstyle-name))
      
      ;; 선형 치수 그리기 시작
      (command "._DIMLINEAR")
    )
    (princ "\n취소되었습니다.")
  )
  
  (princ)
)

;;;; ============================================================================
;;;; 치수 스타일 생성 함수
;;;; ============================================================================
(defun create_dimstyle (style-name /)
  
  ;; ISO-25 기반 설정 적용
  ;; 전체 축척
  (setvar "DIMSCALE" (atof *dim_scale*))
  
  ;; 문자 설정
  (setvar "DIMTXT" (atof *dim_text_height*))        ; 문자 높이
  (setvar "DIMGAP" (atof *dim_text_gap*))           ; 문자와 치수선 간격
  (setvar "DIMTAD" 1)                                ; 문자를 치수선 위에 배치
  (setvar "DIMTIH" 0)                                ; 문자를 항상 수평으로
  (setvar "DIMTOH" 0)                                ; 문자를 항상 수평으로
  
  ;; 화살표 설정
  (setvar "DIMASZ" (atof *dim_arrow_size*))         ; 화살표 크기
  (setvar "DIMBLK" "")                               ; 화살표 스타일 (기본 닫힌 화살표)
  (setvar "DIMBLK1" "")
  (setvar "DIMBLK2" "")
  
  ;; 치수보조선 설정
  (setvar "DIMEXO" (atof *dim_ext_offset*))         ; 치수보조선 간격띄우기
  (setvar "DIMEXE" (atof *dim_ext_extend*))         ; 치수보조선 연장
  (setvar "DIMSE1" 0)                                ; 첫번째 치수보조선 표시
  (setvar "DIMSE2" 0)                                ; 두번째 치수보조선 표시
  
  ;; 치수선 설정
  (setvar "DIMDLI" 10.0)                             ; 치수선 간격
  (setvar "DIMDLE" 0.0)                              ; 치수선 연장
  
  ;; 단위 설정
  (setvar "DIMUNIT" 2)                               ; 십진법
  (setvar "DIMDEC" 2)                                ; 소수점 이하 자릿수
  (setvar "DIMZIN" 8)                                ; 0 표시 제어
  
  ;; 공차 설정
  (setvar "DIMTOL" 0)                                ; 공차 표시 끄기
  
  ;; 색상 설정
  (setvar "DIMCLRD" 0)                               ; 치수선 색상 (ByBlock)
  (setvar "DIMCLRE" 0)                               ; 치수보조선 색상 (ByBlock)
  (setvar "DIMCLRT" 0)                               ; 문자 색상 (ByBlock)
  
  ;; 치수 스타일이 이미 있는지 확인
  (if (tblsearch "DIMSTYLE" style-name)
    (progn
      ;; 있으면 덮어쓰기
      (command "._-DIMSTYLE" "_S" style-name "_Y")
    )
    (progn
      ;; 없으면 새로 생성
      (command "._-DIMSTYLE" "_S" style-name)
    )
  )
  
  ;; 새로 만든 스타일을 현재 스타일로 설정
  (command "._-DIMSTYLE" "_R" style-name)
  
  (princ (strcat "\n치수 스타일 '" style-name "' 생성 완료!"))
  (princ)
)

;;;; ============================================================================
;;;; 프로그램 로드 메시지
;;;; ============================================================================
(princ "\n========================================")
(princ "\n  AutoCAD 치수 스타일 LISP 로드됨")
(princ "\n========================================")
(princ "\n  명령어: MYDIM")
(princ "\n  설명: ISO-25 기반 치수 스타일 생성")
(princ "\n========================================")
(princ)
