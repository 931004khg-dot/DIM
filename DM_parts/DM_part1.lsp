;;;; ============================================================================
;;;; DM.lsp - Part 1 of 3
;;;; Lines: 1-450
;;;; 
;;;; 이 파일은 DM.lsp의 Part 1입니다.
;;;; Part 2, Part 3와 순서대로 합쳐서 사용하세요.
;;;; 
;;;; 합치는 방법:
;;;;   Windows CMD: copy /b DM_part1.lsp+DM_part2.lsp+DM_part3.lsp DM.lsp
;;;;   PowerShell:  Get-Content DM_part1.lsp,DM_part2.lsp,DM_part3.lsp | Set-Content DM.lsp
;;;;   Linux/Mac:   cat DM_part1.lsp DM_part2.lsp DM_part3.lsp > DM.lsp
;;;; ============================================================================

;;;; ============================================================================
;;;; AutoCAD 치수 스타일 생성 LISP 프로그램
;;;; 파일명: DM.lsp
;;;; 명령어: DM
;;;; ============================================================================

(vl-load-com)

(setq *g_dimstyle_lsp_path*
  (if *load-pathname*
    (vl-filename-directory *load-pathname*)
    nil ; 로드 경로를 찾을 수 없음 (예: 붙여넣기 실행)
  )
)

;; 마지막 사용 축척 저장 (전역 변수)
(if (not *last_dim_scale*)
  (setq *last_dim_scale* "20")  ; 기본값: 20
)

;; 마지막 사용 y값 (치수선 거리) 저장 (전역 변수)
(if (not *last_dim_distance*)
  (setq *last_dim_distance* "20")  ; 기본값: 20 (전체 축척에 비례)
)

;; 고급 옵션 저장 (전역 변수)
(if (not *custom_text_height*)
  (setq *custom_text_height* nil)  ; nil = 자동 계산
)
(if (not *custom_arrow_size*)
  (setq *custom_arrow_size* nil)
)
(if (not *custom_ext_offset*)
  (setq *custom_ext_offset* nil)
)
(if (not *custom_ext_extend*)
  (setq *custom_ext_extend* nil)
)
(if (not *custom_text_gap*)
  (setq *custom_text_gap* nil)
)

;;;; ============================================================================
;;;; DCL 파일 생성 함수
;;;; ============================================================================
(defun create_dcl_file (/ dcl_file dcl_path dcl_content)
  
  (if (and *g_dimstyle_lsp_path* (vl-file-directory-p *g_dimstyle_lsp_path*))
    (setq dcl_path (strcat *g_dimstyle_lsp_path* "\\DM.dcl"))
    (progn
      (setq dcl_path (vl-filename-mktemp "dm_dcl" nil ".dcl"))
      (princ (strcat "\nLISP 파일 경로를 찾을 수 없어 임시 폴더에 DCL 생성: " dcl_path))
    )
  )
  
  ;; DCL 내용을 문자열로 정의
  (setq dcl_content (strcat
    "dimstyle : dialog {\n"
    "    label = \"치수 스타일 설정 (DM)\";\n"
    "    : boxed_radio_column {\n"
    "        label = \"도면 타입\";\n"
    "        : radio_button { key = \"detail_1\"; label = \"상세도(1)\"; }\n"
    "        : radio_button { key = \"plan_1000\"; label = \"평면도(1000)\"; }\n"
    "    }\n"
    "    : boxed_column {\n"
    "        label = \"기본 설정 (ISO-25 표준)\";\n"
    "        : boxed_radio_column {\n"
    "            label = \"치수 타입\";\n"
    "            : radio_button { key = \"dim_linear\"; label = \"회전된 치수 (DIMLINEAR)\"; }\n"
    "            : radio_button { key = \"dim_aligned\"; label = \"정렬된 치수 (DIMALIGNED)\"; }\n"
    "        }\n"
    "        : row {\n"
    "            : text { label = \"전체 축척 (DIMSCALE):\"; width = 25; fixed_width = true; }\n"
    "            : edit_box { key = \"dimscale\"; edit_width = 10; value = \"20\"; }\n"
    "        }\n"
    "        : row {\n"
    "            : text { label = \"치수선 거리 (비율):\"; width = 25; fixed_width = true; }\n"
    "            : text { key = \"dim_distance\"; width = 10; fixed_width = true; }\n"
    "        }\n"
    "        : row {\n"
    "            : text { label = \"문자 높이 (기본):\"; width = 25; fixed_width = true; }\n"
    "            : text { key = \"textheight\"; width = 10; fixed_width = true; }\n"
    "        }\n"
    "        : row {\n"
    "            : text { label = \"화살표 크기 (기본):\"; width = 25; fixed_width = true; }\n"
    "            : text { key = \"arrowsize\"; width = 10; fixed_width = true; }\n"
    "        }\n"
    "    }\n"
    "    : boxed_column {\n"
    "        label = \"치수보조선 설정 (기본)\";\n"
    "        : row {\n"
    "            : text { label = \"간격띄우기 (DIMEXO):\"; width = 25; fixed_width = true; }\n"
    "            : text { key = \"extoffset\"; width = 10; fixed_width = true; }\n"
    "        }\n"
    "        : row {\n"
    "            : text { label = \"보조선 연장 (DIMEXE):\"; width = 25; fixed_width = true; }\n"
    "            : text { key = \"extextend\"; width = 10; fixed_width = true; }\n"
    "        }\n"
    "    }\n"
    "    : boxed_column {\n"
    "        label = \"문자 설정 (기본)\";\n"
    "        : row {\n"
    "            : text { label = \"문자 간격 (DIMGAP):\"; width = 25; fixed_width = true; }\n"
    "            : text { key = \"textgap\"; width = 10; fixed_width = true; }\n"
    "        }\n"
    "    }\n"
    "    spacer;\n"
    "    : row {\n"
    "        : spacer { width = 1; }\n"
    "        : button { key = \"create_leader\"; label = \"지시선\"; fixed_width = true; width = 12; }\n"
    "        : button { key = \"edit_advanced\"; label = \"설정\"; fixed_width = true; width = 12; }\n"
    "        : button { key = \"reset\"; label = \"리셋\"; fixed_width = true; width = 12; }\n"
    "        : button { key = \"accept\"; label = \"확인\"; is_default = true; fixed_width = true; width = 12; }\n"
    "        : button { key = \"cancel\"; label = \"취소\"; is_cancel = true; fixed_width = true; width = 12; }\n"
    "        : spacer { width = 1; }\n"
    "    }\n"
    "}\n"
    "advanced_settings : dialog {\n"
    "    label = \"고급 설정\";\n"
    "    : boxed_column {\n"
    "        label = \"기본 설정\";\n"
    "        : row {\n"
    "            : text { label = \"치수선 거리 (비율):\"; width = 25; }\n"
    "            : edit_box { key = \"adv_distance\"; edit_width = 10; }\n"
    "        }\n"
    "    }\n"
    "    : boxed_column {\n"
    "        label = \"문자 설정 (기본)\";\n"
    "        : row {\n"
    "            : text { label = \"문자 높이 (DIMTXT):\"; width = 25; }\n"
    "            : edit_box { key = \"adv_textheight\"; edit_width = 10; }\n"
    "        }\n"
    "        : row {\n"
    "            : text { label = \"문자 간격 (DIMGAP):\"; width = 25; }\n"
    "            : edit_box { key = \"adv_textgap\"; edit_width = 10; }\n"
    "        }\n"
    "    }\n"
    "    : boxed_column {\n"
    "        label = \"화살표 설정 (기본)\";\n"
    "        : row {\n"
    "            : text { label = \"화살표 크기 (DIMASZ):\"; width = 25; }\n"
    "            : edit_box { key = \"adv_arrowsize\"; edit_width = 10; }\n"
    "        }\n"
    "    }\n"
    "    : boxed_column {\n"
    "        label = \"치수보조선 설정 (기본)\";\n"
    "        : row {\n"
    "            : text { label = \"간격띄우기 (DIMEXO):\"; width = 25; }\n"
    "            : edit_box { key = \"adv_extoffset\"; edit_width = 10; }\n"
    "        }\n"
    "        : row {\n"
    "            : text { label = \"보조선 연장 (DIMEXE):\"; width = 25; }\n"
    "            : edit_box { key = \"adv_extextend\"; edit_width = 10; }\n"
    "        }\n"
    "    }\n"
    "    spacer;\n"
    "    : row {\n"
    "        : spacer { width = 1; }\n"
    "        : button { key = \"save\"; label = \"저장\"; is_default = true; fixed_width = true; width = 15; }\n"
    "        : button { key = \"save_as\"; label = \"다른이름으로 저장\"; fixed_width = true; width = 20; }\n"
    "        : button { key = \"cancel\"; label = \"취소\"; is_cancel = true; fixed_width = true; width = 12; }\n"
    "        : spacer { width = 1; }\n"
    "    }\n"
    "}\n"
  ))
  
  (setq dcl_file (open dcl_path "w"))
  (if dcl_file
    (progn
      (write-line dcl_content dcl_file)
      (close dcl_file)
      (princ (strcat "\nDCL 파일 생성됨: " dcl_path))
      dcl_path
    )
    (progn
      (princ "\nDCL 파일 생성 실패!")
      nil
    )
  )
)

;;;; ============================================================================
;;;; 메인 명령어 함수
;;;; ============================================================================
(defun C:DM (/ dcl_id result dimstyle-name dcl_path
                 base_text_height base_arrow_size base_ext_offset 
                 base_ext_extend base_text_gap
                 scale_ratio adv_result continue_loop pt1 pt2 pt3 
                 dx dy dist unit_x unit_y perp_x perp_y mid_x mid_y
                 to_pt3_x to_pt3_y dot_product dim_pt
                 leader_result ; (draw_custom_leader)의 결과를 받기 위한 변수 추가
                )
  (princ "\n치수 스타일 생성 프로그램 시작... (DM)")
  
  ;; DCL 파일 자동 생성
  (setq dcl_path (create_dcl_file))
  
  (if (or (not dcl_path) (not (findfile dcl_path)))
    (progn
      (alert "DCL 파일을 생성하거나 찾을 수 없습니다!")
      (exit)
    )
  )
  
  ;; DCL 파일 로드
  (setq dcl_id (load_dialog dcl_path))
  
  (if (not dcl_id)
    (progn
      (alert (strcat "DCL 대화상자를 로드할 수 없습니다!\n경로: " dcl_path))
      (exit)
    )
  )
  
  (if (not (new_dialog "dimstyle" dcl_id))
    (progn
      (unload_dialog dcl_id)
      (alert "DCL 대화상자를 초기화할 수 없습니다!")
      (exit)
    )
  )
  
  ;; 기본 축척 설정 (마지막 사용 값 불러오기)
  (setq *dim_scale* *last_dim_scale*)
  (setq *dim_distance* *last_dim_distance*)
  
  ;; ISO-25 표준 기준값 (플롯 크기 기준)
  (setq base_text_height 3.0)
  (setq base_arrow_size 2.5)
  (setq base_ext_offset 10.0)
  (setq base_ext_extend 1.25)
  (setq base_text_gap 0.625)  ; 문자 간격 (DIMGAP = 0.625)
  
  ;; DCL 컨트롤 초기화
  (set_tile "dimscale" *dim_scale*)
  (set_tile "dim_distance" *dim_distance*)  ; 읽기 전용 표시
  
  ;; 도면 타입 라디오 버튼 초기화 (기본: 상세도)
  (if (not *drawing_type*)
    (setq *drawing_type* "detail")  ; 기본값: 상세도(1)
  )
  (if (= *drawing_type* "detail")
    (set_tile "detail_1" "1")
    (set_tile "plan_1000" "1")
  )
  
  ;; 치수 타입 라디오 버튼 초기화
  (set_tile "dim_aligned" "1")
  (setq *dim_type* "1")
  
  ;; 기본값 표시
  (if *custom_text_height*
    (set_tile "textheight" (rtos (atof *custom_text_height*) 2 2))
    (set_tile "textheight" (rtos base_text_height 2 2))
  )
  (if *custom_arrow_size*
    (set_tile "arrowsize" (rtos (atof *custom_arrow_size*) 2 2))
    (set_tile "arrowsize" (rtos base_arrow_size 2 2))
  )
  (if *custom_ext_offset*
    (set_tile "extoffset" (rtos (atof *custom_ext_offset*) 2 2))
    (set_tile "extoffset" (rtos base_ext_offset 2 2))
  )
  (if *custom_ext_extend*
    (set_tile "extextend" (rtos (atof *custom_ext_extend*) 2 2))
    (set_tile "extextend" (rtos base_ext_extend 2 2))
  )
  (if *custom_text_gap*
    (set_tile "textgap" (rtos (atof *custom_text_gap*) 2 3))
    (set_tile "textgap" (rtos base_text_gap 2 3))
  )
  
  ;; 액션 설정
  (action_tile "detail_1" "(setq *drawing_type* \"detail\")")
  (action_tile "plan_1000" "(setq *drawing_type* \"plan\")")
  (action_tile "dim_linear" "(setq *dim_type* \"0\")")
  (action_tile "dim_aligned" "(setq *dim_type* \"1\")")
  (action_tile "dimscale" 
    "(progn
       (setq *dim_scale* $value)
       ;; DIMSCALE 변경 시 모든 크기 값 실시간 업데이트
       (setq scale_ratio (atof *dim_scale*))
       ;; 치수선 거리 (비례 적용)
       (set_tile \"dim_distance\" *dim_distance*)
       ;; 문자 높이 (기본 또는 커스텀 값 × DIMSCALE)
       (if *custom_text_height*
         (set_tile \"textheight\" (rtos (atof *custom_text_height*) 2 2))
         (set_tile \"textheight\" (rtos base_text_height 2 2))
       )
       ;; 화살표 크기
       (if *custom_arrow_size*
         (set_tile \"arrowsize\" (rtos (atof *custom_arrow_size*) 2 2))
         (set_tile \"arrowsize\" (rtos base_arrow_size 2 2))
       )
       ;; 간격띄우기
       (if *custom_ext_offset*
         (set_tile \"extoffset\" (rtos (atof *custom_ext_offset*) 2 2))
         (set_tile \"extoffset\" (rtos base_ext_offset 2 2))
       )
       ;; 보조선 연장
       (if *custom_ext_extend*
         (set_tile \"extextend\" (rtos (atof *custom_ext_extend*) 2 2))
         (set_tile \"extextend\" (rtos base_ext_extend 2 2))
       )
       ;; 문자 간격
       (if *custom_text_gap*
         (set_tile \"textgap\" (rtos (atof *custom_text_gap*) 2 3))
         (set_tile \"textgap\" (rtos base_text_gap 2 3))
       )
     )"
  )
  ;; dim_distance는 고급 설정에서만 수정 가능
  (action_tile "create_leader" "(setq result 3) (done_dialog 3)")
  (action_tile "edit_advanced" "(setq result 2) (done_dialog 2)")
  (action_tile "reset"
    "(progn
       (setq *dim_scale* \"20\")
       (setq *dim_distance* \"20\")
       (setq *custom_text_height* nil)
       (setq *custom_arrow_size* nil)
       (setq *custom_ext_offset* nil)
       (setq *custom_ext_extend* nil)
       (setq *custom_text_gap* nil)
       (set_tile \"dimscale\" \"20\")
       (set_tile \"dim_distance\" \"20\")
       (set_tile \"textheight\" (rtos base_text_height 2 2))
       (set_tile \"arrowsize\" (rtos base_arrow_size 2 2))
       (set_tile \"extoffset\" (rtos base_ext_offset 2 2))
       (set_tile \"extextend\" (rtos base_ext_extend 2 2))
       (set_tile \"textgap\" (rtos base_text_gap 2 3))
       (princ \"\\n기본값으로 리셋되었습니다.\")
     )"
  )
  (action_tile "accept" "(done_dialog 1)")
  (action_tile "cancel" "(done_dialog 0)")
  
  ;; 대화상자 표시
  (setq result (start_dialog))
  
  ;; 지시선 버튼을 누른 경우
  (if (= result 3)
    (progn
      (unload_dialog dcl_id)
      
      (setq *last_dim_scale* *dim_scale*)
      (setq *last_dim_distance* *dim_distance*)
      
      ;; 레이어 생성
      (ensure_layer "!-치수" 1)
      (setvar "CLAYER" "!-치수")
      (princ "\n현재 레이어: !-치수")
      
      ;; 기본값 설정
      (if *custom_text_height*
        (setq *dim_text_height* *custom_text_height*)
        (setq *dim_text_height* (rtos base_text_height 2 2))
      )
      (if *custom_arrow_size*
        (setq *dim_arrow_size* *custom_arrow_size*)
        (setq *dim_arrow_size* (rtos base_arrow_size 2 2))
      )
      (if *custom_ext_offset*
        (setq *dim_ext_offset* *custom_ext_offset*)
        (setq *dim_ext_offset* (rtos base_ext_offset 2 2))
      )
      (if *custom_ext_extend*
        (setq *dim_ext_extend* *custom_ext_extend*)
        (setq *dim_ext_extend* (rtos base_ext_extend 2 2))
      )
      (if *custom_text_gap*
        (setq *dim_text_gap* *custom_text_gap*)
        (setq *dim_text_gap* (rtos base_text_gap 2 3))
      )
      
      ;; 치수 스타일 생성 (도면 타입에 따라 이름 결정)
      (if *new_dimstyle_name*
        (setq dimstyle-name *new_dimstyle_name*)
        (if (= *drawing_type* "detail")
          (setq dimstyle-name "ISO-25-Custom")
          (setq dimstyle-name "ISO-25-DWG")
        )
      )
      (create_dimstyle dimstyle-name)
      
      ;; 연속 커스텀 지시선 그리기
      (princ "\n지시선 그리기 시작... (ESC로 종료)")
      (princ (strcat "\n현재 치수 스타일: " dimstyle-name))
      (princ (strcat "\n전체 축척: " *dim_scale*))
      (princ "\n지시선 설정: 도그렉 없음, 문자 맨 아래 행 중간 부착")
      
      (setq continue_loop T)
      (while continue_loop
        (princ "\n\n=== 새 지시선 시작 ===")
        (setq leader_result (draw_custom_leader dimstyle-name))
        
        ;; 사용자가 ESC를 누르거나 취소하면 종료
        (if (not leader_result)
          (progn
            (princ "\n지시선 그리기 종료.")
            (setq continue_loop nil)
          )
          (princ "\n지시선 생성 완료!")
        )
      )
      (princ)
    )
  )
  
  ;; 수정 버튼을 누른 경우
  (while (= result 2)
    (if (not (new_dialog "advanced_settings" dcl_id))
      (progn
        (alert "고급 설정 대화상자를 열 수 없습니다!")
        (setq result 0)
      )
      (progn
        ;; 고급 설정 초기화
        (if *custom_text_height*
          (set_tile "adv_textheight" *custom_text_height*)
          (set_tile "adv_textheight" (rtos base_text_height 2 2))
        )
        (if *custom_arrow_size*
          (set_tile "adv_arrowsize" *custom_arrow_size*)
          (set_tile "adv_arrowsize" (rtos base_arrow_size 2 2))
        )
        (if *custom_ext_offset*
          (set_tile "adv_extoffset" *custom_ext_offset*)
          (set_tile "adv_extoffset" (rtos base_ext_offset 2 2))
        )
        (if *custom_ext_extend*
          (set_tile "adv_extextend" *custom_ext_extend*)
          (set_tile "adv_extextend" (rtos base_ext_extend 2 2))
        )
        (if *custom_text_gap*
          (set_tile "adv_textgap" *custom_text_gap*)
          (set_tile "adv_textgap" (rtos base_text_gap 2 3))
        )
        (set_tile "adv_distance" *dim_distance*)
        
        ;; 고급 설정 액션
        (action_tile "save"
          "(progn
             (setq *dim_distance* (get_tile \"adv_distance\"))
             (setq *custom_text_height* (get_tile \"adv_textheight\"))
             (setq *custom_arrow_size* (get_tile \"adv_arrowsize\"))
             (setq *custom_ext_offset* (get_tile \"adv_extoffset\"))
             (setq *custom_ext_extend* (get_tile \"adv_extextend\"))
             (setq *custom_text_gap* (get_tile \"adv_textgap\"))
             (setq *save_as_mode* nil)
             (done_dialog 3)

;;;; ============================================================================
;;;; End of Part 1
;;;; 다음: Part 2를 이어서 붙이세요
;;;; ============================================================================

