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
           )"
        )
        (action_tile "save_as"
          "(progn
             (setq *dim_distance* (get_tile \"adv_distance\"))
             (setq *custom_text_height* (get_tile \"adv_textheight\"))
             (setq *custom_arrow_size* (get_tile \"adv_arrowsize\"))
             (setq *custom_ext_offset* (get_tile \"adv_extoffset\"))
             (setq *custom_ext_extend* (get_tile \"adv_extextend\"))
             (setq *custom_text_gap* (get_tile \"adv_textgap\"))
             (setq *save_as_mode* T)
             (done_dialog 3)
           )"
        )
        (action_tile "cancel" "(done_dialog 0)")
        
        (setq adv_result (start_dialog))
        
        (if (= adv_result 3)
          (progn
            ;; "다른이름으로 저장" 모드인 경우 스타일명 입력받기
            (if *save_as_mode*
              (progn
                (setq new_style_name (getstring T "\n새 스타일 이름 입력 (예: ISO-25-Custom_2): "))
                (if (and new_style_name (> (strlen new_style_name) 0))
                  (progn
                    (princ (strcat "\n새 스타일 '" new_style_name "' 생성됨"))
                    (setq *new_dimstyle_name* new_style_name)
                  )
                  (progn
                    (princ "\n유효한 이름이 입력되지 않았습니다. ISO-25-Custom 사용")
                    (setq *new_dimstyle_name* nil)
                  )
                )
              )
              (setq *new_dimstyle_name* nil)
            )
            
            (if (not (new_dialog "dimstyle" dcl_id))
              (progn
                (alert "DCL 대화상자를 다시 열 수 없습니다!")
                (setq result 0)
              )
              (progn
                (set_tile "dimscale" *dim_scale*)
                (set_tile "dim_distance" *dim_distance*)
                (set_tile "dim_aligned" "1")
                (setq *dim_type* "1")
                
                (set_tile "textheight" (rtos (atof *custom_text_height*) 2 2))
                (set_tile "arrowsize" (rtos (atof *custom_arrow_size*) 2 2))
                (set_tile "extoffset" (rtos (atof *custom_ext_offset*) 2 2))
                (set_tile "extextend" (rtos (atof *custom_ext_extend*) 2 2))
                (set_tile "textgap" (rtos (atof *custom_text_gap*) 2 3))
                
                (action_tile "dim_linear" "(setq *dim_type* \"0\")")
                (action_tile "dim_aligned" "(setq *dim_type* \"1\")")
                (action_tile "dimscale" "(setq *dim_scale* $value)")
                (action_tile "dim_distance" "(setq *dim_distance* $value)")
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
                
                (setq result (start_dialog))
              )
            )
          )
          (setq result 0)
        )
      )
    )
  )
  
  (unload_dialog dcl_id)
  
  ;; DCL 파일 정리
  (if (not (wcmatch (strcase (vl-filename-base dcl_path)) "DM"))
    (if (findfile dcl_path)
      (progn
        (vl-file-delete dcl_path)
        (princ (strcat "\n임시 DCL 파일 삭제됨: " dcl_path))
      )
    )
  )
  
  ;; 사용자가 확인을 누른 경우
  (if (= result 1)
    (progn
      (princ "\n치수 스타일 생성 중...")
      
      ;; 레이어 생성
      (ensure_layer "!-치수" 1)
      (setvar "CLAYER" "!-치수")
      (princ "\n현재 레이어: !-치수")
      
      (setq *last_dim_scale* *dim_scale*)
      (setq *last_dim_distance* *dim_distance*)
      
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
      
      ;; LEADER는 DIMSTYLE 설정을 자동으로 따름 (별도 설정 불필요)
      
      ;; 연속 치수 그리기
      (princ "\n치수 그리기 시작... (ESC로 종료)")
      (princ (strcat "\n현재 치수 스타일: " dimstyle-name))
      (princ (strcat "\n전체 축척: " *dim_scale*))
      ;; 치수선 거리 출력 (평면도 모드일 경우 0.001배 적용)
      (if (= *drawing_type* "plan")
        (princ (strcat "\n치수선 거리: " (rtos (* (atof *dim_distance*) (atof *dim_scale*) 0.001) 2 6)))
        (princ (strcat "\n치수선 거리: " (rtos (* (atof *dim_distance*) (atof *dim_scale*)) 2 2)))
      )
      
      (setq continue_loop T)
      (while continue_loop
        (setq pt1 (getpoint "\n첫 번째 치수보조선 원점 또는 [ESC 종료]: "))
        
        (if pt1
          (progn
            (setq pt2 (getpoint pt1 "\n두 번째 치수보조선 원점: "))
            
            (if pt2
              (progn
                (setq pt3 (getpoint pt2 "\n치수선 방향 지정: "))
                
                (if pt3
                  (progn
                    (setq dx (- (car pt2) (car pt1)))
                    (setq dy (- (cadr pt2) (cadr pt1)))
                    (setq dist (sqrt (+ (* dx dx) (* dy dy))))
                    
                    (if (> dist 0.001)
                      (progn
                        (setq unit_x (/ dx dist))
                        (setq unit_y (/ dy dist))
                        (setq perp_x (- unit_y))
                        (setq perp_y unit_x)
                        (setq mid_x (/ (+ (car pt1) (car pt2)) 2.0))
                        (setq mid_y (/ (+ (cadr pt1) (cadr pt2)) 2.0))
                        (setq to_pt3_x (- (car pt3) mid_x))
                        (setq to_pt3_y (- (cadr pt3) mid_y))
                        (setq dot_product (+ (* perp_x to_pt3_x) (* perp_y to_pt3_y)))
                        
                        (if (< dot_product 0)
                          (progn
                            (setq perp_x (- perp_x))
                            (setq perp_y (- perp_y))
                          )
                        )
                        
                        ;; 치수선 거리 계산 (평면도 모드일 경우 0.001배 적용)
                        (if (= *drawing_type* "plan")
                          ;; 평면도(1000): Y값 * 0.001배
                          (setq dim_pt (list 
                            (+ mid_x (* perp_x (* (atof *dim_distance*) (atof *dim_scale*) 0.001)))
                            (+ mid_y (* perp_y (* (atof *dim_distance*) (atof *dim_scale*) 0.001)))
                          ))
                          ;; 상세도(1): Y값 그대로
                          (setq dim_pt (list 
                            (+ mid_x (* perp_x (* (atof *dim_distance*) (atof *dim_scale*))))
                            (+ mid_y (* perp_y (* (atof *dim_distance*) (atof *dim_scale*))))
                          ))
                        )
                        
                        (if (= *dim_type* "0")
                          (progn
                            (command "._DIMLINEAR" pt1 pt2 dim_pt)
                            (princ "\n회전된 치수 생성 완료.")
                          )
                          (progn
                            (command "._DIMALIGNED" pt1 pt2 dim_pt)
                            (princ "\n정렬된 치수 생성 완료.")
                          )
                        )
                      )
                      (princ "\n두 점이 너무 가깝습니다.")
                    )
                  )
                  (progn
                    (princ "\n치수 그리기 취소.")
                    (setq continue_loop nil)
                  )
                )
              )
              (progn
                (princ "\n치수 그리기 종료.")
                (setq continue_loop nil)
              )
            )
          )
          (progn
            (princ "\n치수 그리기 종료.")
            (setq continue_loop nil)
          )
        )
      )
    )
    (princ "\n취소되었습니다.")
  )
  
  (princ)
)

;;;; ============================================================================
;;;; ActiveX로 LEADER 객체 생성 함수
;;;; ============================================================================
(defun draw_custom_leader (style-name / pt1 pt2 pt_list last_pt text_str 
                            text_height continue_input leader_ent text_ent 
                            text_handle leader_dxf leader_result text_result
                            debug_mode
                            dxf_72 dxf_73 dxf_11 ; 텍스트 정렬 변수
                            dogleg_length ; 도그렉(수평 지시선) 길이
                            text_width pt3 text_pos vertical_offset ; 텍스트 길이 및 위치 계산용
                            )
  
  (vl-load-com)
  
  ;; 디버그 모드 (T = 활성화, nil = 비활성화)
  (setq debug_mode T)
  
  ;; 텍스트 높이 계산 (치수 스타일과 동일한 로직)
  (setq text_height (atof *dim_text_height*))
  (if (= *drawing_type* "plan")
    ;; 평면도 모드: 3.0 × (7/6) × 0.001 × DIMSCALE
    (setq text_height (* text_height (/ 7.0 6.0) 0.001 (atof *dim_scale*)))
    ;; 상세도 모드: 3.0 × DIMSCALE (기본값에 DIMSCALE 곱함)
    ;; TEXT 엔티티는 최종 크기를 직접 지정해야 함
    (setq text_height (* text_height (atof *dim_scale*)))
  )
  
  ;; 도그렉(수평 지시선) 길이 계산
  ;; 실제 도면 단위 기준으로 고정값 사용
  ;; 평면도: 실제 6000mm = 도면 6mm (1000배 축소)
  ;; 상세도: 실제 10mm = 도면 10mm (1배)
  (if (= *drawing_type* "plan")
    ;; 평면도 모드: 6mm (실제 6000mm)
    (setq dogleg_length 6)
    ;; 상세도 모드: 10mm
    (setq dogleg_length 10)
  )
  
  (if debug_mode
    (progn
      (princ (strcat "\n[DEBUG] 텍스트 높이: " (rtos text_height 2 6)))
      (princ (strcat "\n[DEBUG] 도그렉 길이: " (rtos dogleg_length 2 6)))
    )
  )
  
  ;; 첫 번째 점 입력 (화살표 위치)
  (setq pt1 (getpoint "\n지시선 시작점 (화살표 위치) 선택: "))
  
  (if pt1
    (progn
      (if debug_mode
        (princ (strcat "\n[DEBUG] 시작점: " (vl-prin1-to-string pt1)))
      )
      
      ;; 점 리스트 초기화
      (setq pt_list (list pt1))
      (setq last_pt pt1)
      (setq continue_input T)
      
      ;; 연속 점 입력
      (while continue_input
        (setq pt2 (getpoint last_pt "\n다음 점 선택 (Enter로 종료): "))
        
        (if pt2
          (progn
            (setq pt_list (append pt_list (list pt2)))
            (setq last_pt pt2)
            (if debug_mode
              (princ (strcat "\n[DEBUG] 점 추가: " (vl-prin1-to-string pt2)))
            )
          )
          (setq continue_input nil)
        )
      )
      
      (if debug_mode
        (princ (strcat "\n[DEBUG] 총 점 개수: " (itoa (length pt_list))))
      )
      
      ;; 최소 2개 점 필요
      (if (>= (length pt_list) 2)
        (progn
          ;; 텍스트 입력
          (setq text_str (getstring T "\n지시선 텍스트 입력 (Enter로 텍스트 없음): "))
          
          (if debug_mode
            (princ (strcat "\n[DEBUG] 입력된 텍스트: [" 
                          (if (and text_str (> (strlen text_str) 0)) text_str "없음")
                          "]"))
          )
          
          ;; TEXT 객체가 필요한 경우 먼저 생성 (LEADER가 참조할 수 있도록)
          (if (and text_str (> (strlen text_str) 0))
            (progn
              (if debug_mode
                (princ "\n[DEBUG] TEXT 엔티티 생성 중...")
              )
              
              ;; 여백 계산 (축척에 비례, 문자 길이 무관)
              ;; 평면도: 1mm (실제 1000mm) = 1.0 × 0.001 × DIMSCALE
              ;; 상세도: 1mm × DIMSCALE
              (if (= *drawing_type* "plan")
                (setq margin (* 1.0 0.001 (atof *dim_scale*)))
                (setq margin (* 1.0 (atof *dim_scale*)))
              )
              
              ;; 임시 텍스트 생성하여 실제 너비 측정
              ;; 원점에 임시 TEXT 엔티티 생성
              (setq temp_text_result
                (entmake
                  (list
                    (cons 0 "TEXT")
                    (cons 8 (getvar "CLAYER"))
                    (cons 10 '(0.0 0.0 0.0))  ; 임시 위치
                    (cons 40 text_height)
                    (cons 1 text_str)
                    (cons 7 "Standard")
                    (cons 72 0)  ; 왼쪽 정렬
                    (cons 73 0)  ; 베이스라인
                  )
                )
              )
              
              ;; 임시 텍스트 엔티티 가져오기
              (setq temp_text_ent (entlast))
              
              ;; textbox로 실제 텍스트 바운딩 박스 측정
              (setq text_bbox (textbox (entget temp_text_ent)))
              (setq text_width (- (car (cadr text_bbox)) (car (car text_bbox))))
              
              ;; 임시 텍스트 삭제
              (entdel temp_text_ent)
              
              ;; 어깨선 길이: 실제 텍스트 폭 + 양쪽 여백
              (setq shoulder_length (+ text_width (* margin 2.0)))
              
              (if debug_mode
                (progn
                  (princ (strcat "\n[DEBUG] 실제 텍스트 폭 (textbox 측정): " (rtos text_width 2 6)))
                  (princ (strcat "\n[DEBUG] 여백 (양쪽): " (rtos margin 2 6)))
                  (princ (strcat "\n[DEBUG] 어깨선 길이: " (rtos shoulder_length 2 6)))
                )
              )
              
              ;; 2번째 점 (도그렉 시작점)
              (setq pt2 (last pt_list))
              
              ;; 수직 오프셋 계산 (DIMSCALE에 비례)
              ;; 평면도: 0.0006 × DIMSCALE
              ;; 수직 오프셋 계산
              ;; 치수선: MTEXT 중심 = 가로선 + DIMGAP + (높이/2)
              ;;        MTEXT 밑줄 = MTEXT 중심 - (높이/2) = 가로선 + DIMGAP
              ;; 지시선: TEXT 밑줄 = 가로선 + vertical_offset
              ;; 결론: vertical_offset = DIMGAP (정확히 일치)
              (if (= *drawing_type* "plan")
                ;; 평면도: DIMGAP = 0.625 × 0.001 × DIMSCALE
                (setq vertical_offset (* (atof *dim_text_gap*) 0.001 (atof *dim_scale*)))
                ;; 상세도: 기존 요구사항 유지 (3.6 / 7 × DIMSCALE)
                (setq vertical_offset (* (/ 3.6 7.0) (atof *dim_scale*)))
              )
              
              (if debug_mode
                (princ (strcat "\n[DEBUG] 수직 오프셋 (DIMGAP): " (rtos vertical_offset 2 6)))
              )
              
              ;; 방향 판단: 1번 점 vs 2번 점의 X 좌표 비교
              (if (> (car pt2) (car pt1))
                ;; L-to-R: 텍스트 중앙 정렬
                (progn
                  (setq dxf_72 1) ; 1 = Center
                  ;; 3번째 점: 어깨선 길이만큼 오른쪽으로
                  (setq pt3 (list (+ (car pt2) shoulder_length) (cadr pt2) (caddr pt2)))
                  ;; 텍스트 위치: 어깨선 중앙 (pt2와 pt3의 중간)
                  (setq text_pos (list 
                    (/ (+ (car pt2) (car pt3)) 2.0)
                    (+ (cadr pt2) vertical_offset)
                    (caddr pt2)
                  ))
                  (if debug_mode (princ "\n[DEBUG] 텍스트 정렬: 중앙-밑줄 (L-R)"))
                )
                ;; R-to-L: 텍스트 중앙 정렬
                (progn
                  (setq dxf_72 1) ; 1 = Center
                  ;; 3번째 점: 어깨선 길이만큼 왼쪽으로
                  (setq pt3 (list (- (car pt2) shoulder_length) (cadr pt2) (caddr pt2)))
                  ;; 텍스트 위치: 어깨선 중앙 (pt2와 pt3의 중간)
                  (setq text_pos (list 
                    (/ (+ (car pt2) (car pt3)) 2.0)
                    (+ (cadr pt2) vertical_offset)
                    (caddr pt2)
                  ))
                  (if debug_mode (princ "\n[DEBUG] 텍스트 정렬: 중앙-밑줄 (R-L)"))
                )
              )
              
              ;; 3번째 점을 점 리스트에 추가
              (setq pt_list (append pt_list (list pt3)))
              
              (if debug_mode
                (progn
                  (princ (strcat "\n[DEBUG] 텍스트 폭: " (rtos text_width 2 6)))
                  (princ (strcat "\n[DEBUG] 어깨선 길이: " (rtos shoulder_length 2 6)))
                  (princ (strcat "\n[DEBUG] 텍스트 위치 (어깨선 중앙): " (vl-prin1-to-string text_pos)))
                  (princ (strcat "\n[DEBUG] 3번째 점: " (vl-prin1-to-string pt3)))
                  (princ (strcat "\n[DEBUG] 최종 점 개수: " (itoa (length pt_list))))
                )
              )
              
              ;; --- 텍스트 정렬 로직 완료 ---
              (setq dxf_11 text_pos) ; 정렬 기준점 = 어깨선 중앙
              (setq dxf_73 0) ; 수직 정렬: 0 = BASELINE (밑줄이 정렬점에 위치)
              
              ;; TEXT 엔티티 생성 (마지막 점 위치)
              (setq text_result
                (entmake
                  (list
                    (cons 0 "TEXT")
                    (cons 8 (getvar "CLAYER"))  ; 현재 레이어
                    (cons 10 dxf_11)  ; 삽입점 (정렬점과 동일하게 설정)
                    (cons 40 text_height)  ; 텍스트 높이
                    (cons 1 text_str)  ; 텍스트 문자열
                    (cons 7 "Standard")  ; 텍스트 스타일
                    (cons 62 7)  ; 색상: 7=흰색 (white)
                    (cons 72 dxf_72) ; [수정] 수평 정렬
                    (cons 73 dxf_73) ; [수정] 수직 정렬
                    (cons 11 dxf_11) ; [수정] 정렬점 (필수)
                  )
                )
              )
              
              (if debug_mode
                (princ (strcat "\n[DEBUG] TEXT entmake 결과: " (vl-prin1-to-string text_result)))
              )
              
              ;; 방금 생성한 TEXT 엔티티 이름 가져오기
              (setq text_ent (entlast))
              
              (if debug_mode
                (progn
                  (princ (strcat "\n[DEBUG] TEXT 엔티티 이름: " (vl-prin1-to-string text_ent)))
                  (if text_ent
                    (princ (strcat "\n[DEBUG] TEXT 엔티티 데이터: " (vl-prin1-to-string (entget text_ent))))
                  )
                )
              )
            )
            ;; 텍스트가 없으면 기본 도그렉 길이만큼 3번째 점 추가
            (progn
              (if debug_mode
                (princ "\n[DEBUG] 텍스트 없음 - 기본 도그렉 길이 사용")
              )
              
              (setq pt2 (last pt_list))
              
              ;; 방향 판단
              (if (> (car pt2) (car pt1))
                ;; L-to-R: 오른쪽으로 dogleg_length
                (setq pt3 (list (+ (car pt2) dogleg_length) (cadr pt2) (caddr pt2)))
                ;; R-to-L: 왼쪽으로 dogleg_length
                (setq pt3 (list (- (car pt2) dogleg_length) (cadr pt2) (caddr pt2)))
              )
              
              ;; 3번째 점 추가
              (setq pt_list (append pt_list (list pt3)))
              
              (if debug_mode
                (princ (strcat "\n[DEBUG] 3번째 점 추가 (기본 도그렉): " (vl-prin1-to-string pt3)))
              )
            )
          )
          
          ;; LEADER 엔티티 생성 (entmake 사용)
          (if debug_mode
            (princ "\n[DEBUG] LEADER DXF 데이터 구성 중...")
          )
          
          (setq leader_dxf
            (append
              (list
                (cons 0 "LEADER")
                (cons 100 "AcDbEntity")  ; 필수: 엔티티 서브클래스
                (cons 8 (getvar "CLAYER"))  ; 현재 레이어
                (cons 100 "AcDbLeader")  ; 필수: LEADER 서브클래스
                (cons 3 style-name)  ; 치수 스타일명
                (cons 71 1)  ; 화살표 있음
                (cons 72 0)  ; 직선 타입 (0=직선, 1=스플라인)
                (cons 73 0)  ; 주석 타입 (0=없음) - 텍스트는 별도 엔티티로 관리
                (cons 74 1)  ; 화살표 헤드 타입: 1=화살표
                (cons 75 0)  ; 후크라인: 0=없음
                (cons 76 (length pt_list))  ; 정점 개수 (이제 3개)
                (cons 40 0.0)  ; 텍스트 주석 높이
                (cons 41 0.0)  ; 텍스트 주석 너비
                (cons 210 (list 0.0 0.0 1.0)) ; 법선 벡터 (Z축)
              )
              ;; TEXT 엔티티는 별도로 관리하므로 340 코드 제거
              nil
              ;; 점 좌표 추가 (10 = 각 정점)
              (mapcar '(lambda (pt) (cons 10 pt)) pt_list)
            )
          )
          
          (if debug_mode
            (princ (strcat "\n[DEBUG] LEADER DXF 데이터: " (vl-prin1-to-string leader_dxf)))
          )
          
          ;; LEADER 생성
          (if debug_mode
            (princ "\n[DEBUG] LEADER entmake 실행 중...")
          )
          
          (setq leader_result (entmake leader_dxf))
          
          (if debug_mode
            (progn
              (princ (strcat "\n[DEBUG] LEADER entmake 결과: " (vl-prin1-to-string leader_result)))
              (if leader_result
                (progn
                  (setq leader_ent (entlast))
                  (princ (strcat "\n[DEBUG] LEADER 엔티티 이름: " (vl-prin1-to-string leader_ent)))
                  (if leader_ent
                    (princ (strcat "\n[DEBUG] LEADER 엔티티 데이터: " (vl-prin1-to-string (entget leader_ent))))
                  )
                )
                (princ "\n[DEBUG] LEADER 생성 실패!")
              )
            )
          )
          
          ;; 메시지 출력
          (if leader_result
            (progn
              (if (and text_str (> (strlen text_str) 0))
                (princ (strcat "\n[OK] 지시선 생성 성공: " (itoa (length pt_list)) "개 점, 텍스트: " text_str))
                (princ (strcat "\n[OK] 지시선 생성 성공: " (itoa (length pt_list)) "개 점, 텍스트 없음"))
              )
              T  ; 성공
            )
            (progn
              (princ "\n[FAIL] 지시선 생성 실패!")
              nil  ; 실패
            )
          )
        )
        (progn
          (princ "\n최소 2개 점이 필요합니다.")
          nil  ; 실패
        )
      )
    )
    nil  ; 취소
  )
)

;;;; ============================================================================
;;;; 레이어 생성/확인 함수
;;;; ============================================================================
(defun ensure_layer (layer-name layer-color /)
  (if (not (tblsearch "LAYER" layer-name))
    (progn
      (command "._-LAYER" "_N" layer-name "_C" layer-color layer-name "")
      (princ (strcat "\n레이어 '" layer-name "' 생성됨 (색상: " (itoa layer-color) ")"))
    )
    (princ (strcat "\n레이어 '" layer-name "' 이미 존재함"))
  )
)

;;;; ============================================================================
;;;; 치수 스타일 생성 함수
;;;; ============================================================================
(defun create_dimstyle (style-name / final_text_height final_arrow_size final_ext_offset final_ext_extend final_text_gap
                                    old_dimstyle old_dimanno)
  
  ;; 현재 치수 스타일이 주석(Annotative)인지 확인
  (setq old_dimstyle (getvar "DIMSTYLE"))
  (setq old_dimanno (getvar "DIMANNO"))
  
  (if (= old_dimanno 1)
    (progn
      (princ (strcat "\n현재 스타일(" old_dimstyle ")이 주석입니다. 'Standard'로 임시 변경..."))
      ;; 'Standard' 스타일이 존재하는지 확인
      (if (not (tblsearch "DIMSTYLE" "Standard"))
        (progn
          ;; Standard 스타일이 없으면 오류 메시지 후 종료 (치명적 오류)
          (alert "치명적 오류: 'Standard' 치수 스타일을 찾을 수 없습니다!\n'Standard' 스타일을 복구한 후 다시 시도하세요.")
          (exit)
        )
        ;; Standard 스타일이 있으면 복원 (이 스타일을 기반으로 설정 변경)
        (command "._-DIMSTYLE" "_R" "Standard")
      )
    )
  )
  
  ;; 평면도(1000) 모드인 경우 축척 조정
  (if (= *drawing_type* "plan")
    (progn
      ;; 텍스트 높이: 7/6 * 0.001배 (DIMSCALE은 setvar로 별도 적용)
      (setq final_text_height (* (atof *dim_text_height*) (/ 7.0 6.0) 0.001))
      ;; 화살표 크기: 8/5 * 0.001배
      (setq final_arrow_size (* (atof *dim_arrow_size*) (/ 8.0 5.0) 0.001))
      ;; 나머지: 0.001배
      (setq final_ext_offset (* (atof *dim_ext_offset*) 0.001))
      (setq final_ext_extend (* (atof *dim_ext_extend*) 0.001))
      (setq final_text_gap (* (atof *dim_text_gap*) 0.001))
    )
    (progn
      ;; 상세도(1) 모드: 기본값 그대로 사용 (DIMSCALE이 자동 적용됨)
      ;; 평면도와 달리 1:1 비율이므로 0.001배 같은 특수 계수 불필요
      (setq final_text_height (atof *dim_text_height*))
      (setq final_arrow_size (atof *dim_arrow_size*))
      (setq final_ext_offset (atof *dim_ext_offset*))
      (setq final_ext_extend (atof *dim_ext_extend*))
      (setq final_text_gap (atof *dim_text_gap*))
    )
  )
  
  ;; 치수선 설정
  (setvar "DIMCLRD" 256)
  (setvar "DIMDLE" 0.0)
  (setvar "DIMDLI" 3.75)
  
  ;; 치수보조선 설정
  (setvar "DIMCLRE" 256)
  (setvar "DIMEXE" final_ext_extend)
  (setvar "DIMEXO" final_ext_offset)
  (setvar "DIMSE1" 0)
  (setvar "DIMSE2" 0)
  
  ;; 화살촉 설정: 닫고채움 (ClosedFilled)
  ;; AutoCAD 기본 '닫고 채움' 화살표(.)로 설정
  (vl-catch-all-apply 'setvar (list "DIMBLK" "."))
  (vl-catch-all-apply 'setvar (list "DIMBLK1" "."))
  (vl-catch-all-apply 'setvar (list "DIMBLK2" "."))
  (vl-catch-all-apply 'setvar (list "DIMLDRBLK" "."))  ; 지시선 화살표
  (setvar "DIMASZ" final_arrow_size)
  (setvar "DIMCEN" 2.5)
  
  ;; 문자 설정
  (setvar "DIMTXSTY" "Standard")
  (setvar "DIMCLRT" 7)
  (setvar "DIMTXT" final_text_height)
  (setvar "DIMTFAC" 1.0)
  (setvar "DIMTAD" 1) ; [중요] 1 = 수평 지시선 사용 (Leader의 Landing)
  (setvar "DIMJUST" 0)
  (setvar "DIMGAP" final_text_gap)
  (setvar "DIMTIH" 0)
  (setvar "DIMTOH" 0)
  
  ;; 맞춤 설정
  (setvar "DIMATFIT" 2)
  (setvar "DIMTMOVE" 1)
  (setvar "DIMSCALE" (atof *dim_scale*))
  (setvar "DIMTOFL" 1)
  
  ;; 단위 설정
  (setvar "DIMLUNIT" 6)
  (setvar "DIMDEC" 2)
  (setvar "DIMDSEP" ".")
  (setvar "DIMRND" 0)
  (setvar "DIMLFAC" 1.0)
  (setvar "DIMZIN" 8)
  (setvar "DIMAUNIT" 0)
  (setvar "DIMADEC" 0)
  (setvar "DIMAZIN" 0)
  
  ;; 대체 단위
  (setvar "DIMALT" 0)
  (setvar "DIMALTF" 0.03937007)
  (setvar "DIMALTD" 3)
  (setvar "DIMALTRND" 0)
  
  ;; 공차
  (setvar "DIMTOL" 0)
  (setvar "DIMTP" 0)
  (setvar "DIMTM" 0)
  (setvar "DIMTFAC" 1.0)
  (setvar "DIMTOLJ" 0)
  
  ;; 치수 스타일 저장 (재정의 자동 허용)
  (command "._-DIMSTYLE" "_S" style-name "_Y")
  
  (command "._-DIMSTYLE" "_R" style-name)
  
  (princ (strcat "\n치수 스타일 '" style-name "' 생성 완료!"))
  (princ "\n=== ISO-25 표준 설정 적용됨 ===")
  (princ (strcat "\n  도면 타입: " (if (= *drawing_type* "detail") "상세도(1)" "평면도(1000)")))
  (princ (strcat "\n  전체 축척: " *dim_scale*))
  (princ (strcat "\n  문자 높이: " (rtos final_text_height 2 6)))
  (princ (strcat "\n  화살표 크기: " (rtos final_arrow_size 2 6)))
  (princ (strcat "\n  연장선 오프셋: " (rtos final_ext_offset 2 6)))
  (princ (strcat "\n  연장선 길이: " (rtos final_ext_extend 2 6)))
  (princ (strcat "\n  텍스트 간격: " (rtos final_text_gap 2 6)))
  (princ)
)

;;;; ============================================================================
;;;; 프로그램 로드 메시지
;;;; ============================================================================
(princ "\nDM 로드됨")
(if *last_dim_scale*
  (princ (strcat " (마지막 축척: " *last_dim_scale* ")"))
)
(princ)
