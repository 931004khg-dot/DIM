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
                            gap_correction ; [수정] DIMGAP 보정값
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
  
  ;; [수정] 오른쪽-왼쪽(R-L) 그리기 시 꼬리 보정값 계산 (동적 계산)
  ;; AutoCAD LEADER는 R->L 방향으로 그릴 때 DIMGAP(문자 간격)만큼의 
  ;; 착륙선(Landing)을 강제로 생성하는 특성이 있음 (EXPLODE시 확인됨)
  ;; 따라서 이 DIMGAP 길이만큼 미리 빼줘야(오른쪽으로 당겨야) 정확한 길이가 나옴
  
  (if (= *drawing_type* "plan")
    ;; 평면도: DIMGAP * 0.001 * Scale
    (setq gap_correction (* (atof *dim_text_gap*) 0.001 (atof *dim_scale*)))
    ;; 상세도: DIMGAP * Scale (단순히 DIMGAP만 쓰면 축척 대응 안됨)
    (setq gap_correction (* (atof *dim_text_gap*) (atof *dim_scale*)))
  )
  
  (if debug_mode
    (progn
      (princ (strcat "\n[DEBUG] 텍스트 높이: " (rtos text_height 2 6)))
      (princ (strcat "\n[DEBUG] 도그렉 길이: " (rtos dogleg_length 2 6)))
      (princ (strcat "\n[DEBUG] DIMGAP 보정값(축척적용): " (rtos gap_correction 2 6)))
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
          ;; 텍스트 입력 또는 엔티티 선택
          ;; 1단계: 엔티티 선택 우선 시도
          (setq input_result (nentselp "\n복사할 TEXT 선택 (Enter=키보드 입력): "))
          
          (if input_result
            ;; 엔티티가 선택됨 - 텍스트 복사
            (progn
              (setq selected_ent (car input_result))
              (setq ent_data (entget selected_ent))
              (setq ent_type (cdr (assoc 0 ent_data)))
              
              (cond
                ;; TEXT 엔티티
                ((= ent_type "TEXT")
                  (setq text_str (cdr (assoc 1 ent_data)))
                  (princ (strcat "\n복사된 텍스트: \"" text_str "\""))
                )
                ;; MTEXT 엔티티
                ((= ent_type "MTEXT")
                  (setq text_str (cdr (assoc 1 ent_data)))
                  ;; MTEXT는 여러 DXF 3 그룹이 있을 수 있음
                  (setq temp_data ent_data)
                  (while (setq next_text (cdr (assoc 3 temp_data)))
                    (setq text_str (strcat text_str next_text))
                    (setq temp_data (cdr (member (assoc 3 temp_data) temp_data)))
                  )
                  (princ (strcat "\n복사된 텍스트: \"" text_str "\""))
                )
                ;; 다른 엔티티 타입 - 키보드 입력으로 전환
                (T
                  (princ (strcat "\n선택한 엔티티는 TEXT 또는 MTEXT가 아닙니다: " ent_type))
                  (setq text_str (getstring T "\n지시선 텍스트 입력 (Enter=텍스트 없음): "))
                )
              )
            )
            ;; 엔티티 선택 안됨 (Enter 누름) - 키보드 입력 모드
            (progn
              (setq text_str (getstring T "지시선 텍스트 입력 (Enter=텍스트 없음): "))
              (if (and text_str (> (strlen text_str) 0))
                (princ (strcat "\n입력된 텍스트: \"" text_str "\""))
                (princ "\n텍스트 없이 지시선 생성")
              )
            )
          )
          
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
