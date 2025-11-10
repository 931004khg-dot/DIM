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
                 final_text_height final_arrow_size final_text_gap
                 scale_ratio adv_result continue_loop pt1 pt2 pt3 
                 dx dy dist unit_x unit_y perp_x perp_y mid_x mid_y
                 to_pt3_x to_pt3_y dot_product dim_pt
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
  (setq base_text_gap 0.5)  ; MLEADER 착지 간격 (스크린샷 2: 0.5)
  
  ;; DCL 컨트롤 초기화
  (set_tile "dimscale" *dim_scale*)
  (set_tile "dim_distance" *dim_distance*)  ; 읽기 전용 표시
  
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
      
      ;; 기본값 설정
      (if *custom_text_height*
        (setq *dim_text_height* *custom_text_height*)
        (setq *dim_text_height* (rtos base_text_height 2 2))
      )
      (if *custom_arrow_size*
        (setq *dim_arrow_size* *custom_arrow_size*)
        (setq *dim_arrow_size* (rtos base_arrow_size 2 2))
      )
      (if *custom_text_gap*
        (setq *dim_text_gap* *custom_text_gap*)
        (setq *dim_text_gap* (rtos base_text_gap 2 3))
      )

      ;; MLEADER 최종 크기 계산 (기본 크기 * 전체 축척)
      (setq final_text_height (* (atof *dim_text_height*) (atof *dim_scale*)))
      (setq final_arrow_size (* (atof *dim_arrow_size*) (atof *dim_scale*)))
      (setq final_text_gap (* (atof *dim_text_gap*) (atof *dim_scale*)))
      
      ;; MLEADER 스타일 생성 (대화상자 표시)
      (create_mleader_style "ISO-25-Custom" 
                            final_text_height
                            final_arrow_size
                            final_text_gap
      )
      
      ;; 지시선 그리기
      (princ "\n지시선 그리기 모드...")
      (command "._MLEADER")
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
      
      ;; 치수 스타일 생성 (다른이름으로 저장한 경우 새 이름 사용)
      (setq dimstyle-name (if *new_dimstyle_name* *new_dimstyle_name* "ISO-25-Custom"))
      (create_dimstyle dimstyle-name)
      
      ;; MLEADER 최종 크기 계산
      (setq final_text_height (* (atof *dim_text_height*) (atof *dim_scale*)))
      (setq final_arrow_size (* (atof *dim_arrow_size*) (atof *dim_scale*)))
      (setq final_text_gap (* (atof *dim_text_gap*) (atof *dim_scale*)))
      
      ;; MLEADER 스타일 생성 (대화상자 표시)
      (create_mleader_style dimstyle-name 
                            final_text_height
                            final_arrow_size
                            final_text_gap
      )
      
      ;; 연속 치수 그리기
      (princ "\n치수 그리기 시작... (ESC로 종료)")
      (princ (strcat "\n현재 치수 스타일: " dimstyle-name))
      (princ (strcat "\n전체 축척: " *dim_scale*))
      (princ (strcat "\n치수선 거리: " (rtos (* (atof *dim_distance*) (atof *dim_scale*)) 2 2)))
      
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
                        
                        ;; 치수선 거리 = dim_distance * DIMSCALE (비례 적용)
                        (setq dim_pt (list 
                          (+ mid_x (* perp_x (* (atof *dim_distance*) (atof *dim_scale*))))
                          (+ mid_y (* perp_y (* (atof *dim_distance*) (atof *dim_scale*))))
                        ))
                        
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
;;;; MLEADER 스타일 생성 함수 (Dictionary 기반 + entmake)
;;;; ============================================================================
(defun create_mleader_style (style-name final-text-height final-arrow-size final-text-gap / 
                             old_cmdecho old_osmode mleader_dict style_dict new_style
                             standard_style style_data xdata_list
                            )
  (princ (strcat "\nMLEADER 스타일 '" style-name "' 생성 중..."))
  
  ;; 환경 변수 저장
  (setq old_cmdecho (getvar "CMDECHO"))
  (setq old_osmode (getvar "OSMODE"))
  (setvar "CMDECHO" 0)
  (setvar "OSMODE" 0)
  
  ;; ACAD_MLEADERSTYLE dictionary 가져오기
  (setq mleader_dict (dictsearch (namedobjdict) "ACAD_MLEADERSTYLE"))
  
  (if mleader_dict
    (progn
      ;; Standard 스타일의 정의 가져오기
      (setq style_dict (namedobjdict))
      (setq standard_style (dictsearch 
                             (cdr (assoc -1 mleader_dict)) 
                             "Standard"))
      
      (if standard_style
        (progn
          (princ "\n  Standard 스타일 찾음, 복사 시도...")
          
          ;; Standard 기반으로 새 스타일 데이터 생성
          (setq style_data
            (list
              (cons 0 "MLEADERSTYLE")
              (cons 100 "AcDbMLeaderStyle")
              (cons 2 style-name)
              (cons 3 "")
              (cons 70 0)
              (cons 71 1)  ; Content type: 1=MText
              (cons 170 2)  ; Text attachment direction
              (cons 171 1)  ; Text attachment type
              (cons 172 0)  ; Text angle type
              (cons 90 2)  ; Text color (ByLayer)
              (cons 40 final-text-height)  ; Text height
              (cons 41 final-arrow-size)  ; Arrow size
              (cons 140 (* 0.36 (atof *dim_scale*)))  ; Dogleg length
              (cons 145 final-text-gap)  ; Landing gap
              (cons 174 1)  ; Arrow head symbol (Closed filled)
              (cons 175 1)  ; Block connection type
              (cons 176 0)  ; Enable landing
              (cons 177 0)  ; Enable dogleg
              (cons 178 1)  ; Max leader points
              (cons 340 (cdr (assoc 7 standard_style)))  ; Text style object ID
            )
          )
          
          ;; entmake로 새 MLEADERSTYLE 생성 시도
          (if (setq new_style (entmake style_data))
            (progn
              (setvar "CMLEADERSTYLE" style-name)
              (princ (strcat "\n  MLEADER 스타일 '" style-name "' 자동 생성 완료!"))
              (princ (strcat "\n  문자 높이: " (rtos final-text-height 2 2)))
              (princ (strcat "\n  화살표 크기: " (rtos final-arrow-size 2 2)))
              (princ (strcat "\n  착지 간격: " (rtos final-text-gap 2 2)))
              (princ (strcat "\n  연결선 거리: " (rtos (* 0.36 (atof *dim_scale*)) 2 2)))
            )
            (progn
              (princ "\n  entmake 실패 - 수동 설정 필요")
              (print_manual_instructions style-name final-text-height final-arrow-size final-text-gap)
            )
          )
        )
        (progn
          (princ "\n  Standard 스타일을 찾을 수 없음")
          (print_manual_instructions style-name final-text-height final-arrow-size final-text-gap)
        )
      )
    )
    (progn
      (princ "\n  MLEADERSTYLE dictionary를 찾을 수 없음")
      (print_manual_instructions style-name final-text-height final-arrow-size final-text-gap)
    )
  )
  
  ;; 환경 변수 복원
  (setvar "CMDECHO" old_cmdecho)
  (setvar "OSMODE" old_osmode)
  
  (princ)
)

;; 수동 설정 안내 출력 함수
(defun print_manual_instructions (style-name final-text-height final-arrow-size final-text-gap)
  (princ "\n  ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
  (princ "\n  자동 생성 실패 - 수동 설정 가이드:")
  (princ "\n  ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
  (princ (strcat "\n  1. MLEADERSTYLE 명령어 입력"))
  (princ (strcat "\n  2. '새로 만들기' 클릭"))
  (princ (strcat "\n  3. 이름: " style-name))
  (princ (strcat "\n  4. '수정' 클릭 후 다음 값 설정:"))
  (princ "\n  ")
  (princ (strcat "\n  [지시선 형식 탭]"))
  (princ (strcat "\n    • 자동 연결선 포함: 체크"))
  (princ (strcat "\n    • 연결선 거리: " (rtos (* 0.36 (atof *dim_scale*)) 2 2)))
  (princ (strcat "\n    • 최대 지시선 점 수: 2"))
  (princ "\n  ")
  (princ (strcat "\n  [지시선 구조 탭]"))
  (princ (strcat "\n    • 문자 높이: " (rtos final-text-height 2 2)))
  (princ (strcat "\n    • 착지 간격: " (rtos final-text-gap 2 2)))
  (princ "\n  ")
  (princ (strcat "\n  [내용 탭]"))
  (princ (strcat "\n    • 화살표 크기: " (rtos final-arrow-size 2 2)))
  (princ "\n  ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
  (princ)
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
(defun create_dimstyle (style-name /)
  
  ;; 치수선 설정
  (setvar "DIMCLRD" 256)
  (setvar "DIMDLE" 0.0)
  (setvar "DIMDLI" 3.75)
  
  ;; 치수보조선 설정
  (setvar "DIMCLRE" 256)
  (setvar "DIMEXE" (atof *dim_ext_extend*))
  (setvar "DIMEXO" (atof *dim_ext_offset*))
  (setvar "DIMSE1" 0)
  (setvar "DIMSE2" 0)
  
  ;; 화살촉 설정
  (setvar "DIMBLK" "")
  (setvar "DIMBLK1" "")
  (setvar "DIMBLK2" "")
  (setvar "DIMASZ" (atof *dim_arrow_size*))
  (setvar "DIMCEN" 2.5)
  
  ;; 문자 설정
  (setvar "DIMTXSTY" "Standard")
  (setvar "DIMCLRT" 7)
  (setvar "DIMTXT" (atof *dim_text_height*))
  (setvar "DIMTFAC" 1.0)
  (setvar "DIMTAD" 1)
  (setvar "DIMJUST" 0)
  (setvar "DIMGAP" (atof *dim_text_gap*))
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
  (princ (strcat "\n  전체 축척: " *dim_scale*))
  (princ (strcat "\n  문자 높이 (기본): " *dim_text_height*))
  (princ (strcat "\n  화살표 크기 (기본): " *dim_arrow_size*))
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
