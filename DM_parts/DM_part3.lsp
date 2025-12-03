              )
              
              ;; 2번째 점 (도그렉 시작점)
              (setq pt2 (last pt_list))
              
              ;; 수직 오프셋 계산 (DIMSCALE에 비례)
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
                ;; L-to-R: 오른쪽으로 진행 (화살표가 왼쪽, 꼬리가 오른쪽)
                ;; 이 방향은 AutoCAD가 추가적인 DIMGAP 세그먼트를 시각적으로 숨기거나 생성하지 않음
                (progn
                  (setq dxf_72 1) ; 1 = Center
                  ;; 3번째 점: 어깨선 길이만큼 오른쪽으로
                  (setq pt3 (list (+ (car pt2) shoulder_length) (cadr pt2) (caddr pt2)))
                  ;; 텍스트 위치: 어깨선 중앙
                  (setq text_pos (list 
                    (/ (+ (car pt2) (car pt3)) 2.0)
                    (+ (cadr pt2) vertical_offset)
                    (caddr pt2)
                  ))
                  (if debug_mode (princ "\n[DEBUG] 텍스트 정렬: 중앙-밑줄 (L-R, 보정 없음)"))
                )
                ;; R-to-L: 왼쪽으로 진행 (화살표가 오른쪽, 꼬리가 왼쪽)
                ;; 이 방향에서 AutoCAD는 지시선 끝에 DIMGAP만큼의 '착륙선'을 강제로 덧붙임
                ;; 따라서 우리가 그릴 선은 [원래 길이 - DIMGAP] 이어야 함
                (progn
                  (setq dxf_72 1) ; 1 = Center
                  
                  ;; 텍스트 위치는 '시각적'으로 보이는 전체 길이를 기준으로 계산해야 함
                  ;; 시각적 끝점(Visual Tip) = pt2 - shoulder_length
                  ;; 따라서 텍스트 위치는 (pt2 + (pt2 - shoulder_length)) / 2
                  (setq text_pos (list 
                    (- (car pt2) (/ shoulder_length 2.0))
                    (+ (cadr pt2) vertical_offset)
                    (caddr pt2)
                  ))
                  
                  ;; 3번째 점 (LEADER 엔티티의 실제 끝점)
                  ;; 왼쪽으로 가되, gap_correction(DIMGAP*Scale)만큼 '덜' 감 (오른쪽으로 보정)
                  (setq pt3 (list 
                    (+ (- (car pt2) shoulder_length) gap_correction) 
                    (cadr pt2) 
                    (caddr pt2)
                  ))
                  
                  (if debug_mode (princ "\n[DEBUG] 텍스트 정렬: 중앙-밑줄 (R-L, DIMGAP 길이만큼 보정됨)"))
                )
              )
              
              ;; 3번째 점을 점 리스트에 추가
              (setq pt_list (append pt_list (list pt3)))
              
              (if debug_mode
                (progn
                  (princ (strcat "\n[DEBUG] 텍스트 폭: " (rtos text_width 2 6)))
                  (princ (strcat "\n[DEBUG] 어깨선 길이: " (rtos shoulder_length 2 6)))
                  (princ (strcat "\n[DEBUG] 텍스트 위치: " (vl-prin1-to-string text_pos)))
                  (princ (strcat "\n[DEBUG] 3번째 점 (보정 적용됨): " (vl-prin1-to-string pt3)))
                )
              )
              
              ;; --- 텍스트 정렬 로직 완료 ---
              (setq dxf_11 text_pos) ; 정렬 기준점
              (setq dxf_73 0) ; 수직 정렬: 0 = BASELINE
              
              ;; TEXT 엔티티 생성 (마지막 점 위치)
              (setq text_result
                (entmake
                  (list
                    (cons 0 "TEXT")
                    (cons 8 (getvar "CLAYER"))  ; 현재 레이어
                    (cons 10 dxf_11)  ; 삽입점
                    (cons 40 text_height)  ; 텍스트 높이
                    (cons 1 text_str)  ; 텍스트 문자열
                    (cons 7 "Standard")  ; 텍스트 스타일
                    (cons 62 7)  ; 색상: 7=흰색 (white)
                    (cons 72 dxf_72) ; 수평 정렬
                    (cons 73 dxf_73) ; 수직 정렬
                    (cons 11 dxf_11) ; 정렬점
                  )
                )
              )
              
              ;; 방금 생성한 TEXT 엔티티 이름 가져오기
              (setq text_ent (entlast))
            )
            ;; 텍스트가 없으면 기본 도그렉 길이만큼 3번째 점 추가
            (progn
              (if debug_mode
                (princ "\n[DEBUG] 텍스트 없음 - 기본 도그렉 길이 사용")
              )
              
              (setq pt2 (last pt_list))
              
              ;; 방향 판단 및 보정 적용
              (if (> (car pt2) (car pt1))
                ;; L-to-R: 보정 없음
                (setq pt3 (list (+ (car pt2) dogleg_length) (cadr pt2) (caddr pt2)))
                ;; R-to-L: 보정 적용 (오른쪽으로 gap_correction 만큼 이동)
                (setq pt3 (list (+ (- (car pt2) dogleg_length) gap_correction) (cadr pt2) (caddr pt2)))
              )
              
              ;; 3번째 점 추가
              (setq pt_list (append pt_list (list pt3)))
              
              (if debug_mode
                (princ (strcat "\n[DEBUG] 3번째 점 추가 (기본 도그렉, 보정됨): " (vl-prin1-to-string pt3)))
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
