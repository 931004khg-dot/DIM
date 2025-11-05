;;;; ============================================================================
;;;; AutoCAD 치수 스타일 생성 LISP 프로그램
;;;; 파일명: dimstyle.lsp
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

;; 마지막 사용 치수 타입 저장 (전역 변수)
(if (not *last_dim_type*)
  (setq *last_dim_type* "0")  ; 기본값: 0 = 회전된 치수 (DIMLINEAR)
)

;;;; ============================================================================
;;;; DCL 파일 생성 함수
;;;; ============================================================================
(defun create_dcl_file (/ dcl_file dcl_path)
  
  (if (and *g_dimstyle_lsp_path* (vl-file-directory-p *g_dimstyle_lsp_path*))
    (setq dcl_path (strcat *g_dimstyle_lsp_path* "\\dimstyle.dcl")) ; 윈도우 경로 구분자 사용
    
    (progn
      (setq dcl_path (vl-filename-mktemp "dimstyle_dcl" nil ".dcl"))
      (princ (strcat "\nLISP 파일 경로를 찾을 수 없어 임시 폴더에 DCL 생성: " dcl_path))
    )
  )
  
  (setq dcl_file (open dcl_path "w"))
  
  (if dcl_file
    (progn
      ;; DCL 내용 작성
      (write-line "// ============================================================================" dcl_file)
      (write-line "// AutoCAD 치수 스타일 DCL 대화상자" dcl_file)
      (write-line "// 자동 생성됨" dcl_file)
      (write-line "// ============================================================================" dcl_file)
      (write-line "" dcl_file)
      (write-line "dimstyle : dialog {" dcl_file)
      (write-line "    label = \"ISO-25 치수 스타일 설정\";" dcl_file)
      (write-line "    " dcl_file)
      (write-line "    : boxed_column {" dcl_file)
      (write-line "        label = \"기본 설정 (ISO-25 표준)\";" dcl_file)
      (write-line "        " dcl_file)
      (write-line "        // 치수 타입 선택" dcl_file)
      (write-line "        : boxed_radio_column {" dcl_file)
      (write-line "            label = \"치수 타입\";" dcl_file)
      (write-line "            : radio_button {" dcl_file)
      (write-line "                key = \"dim_linear\";" dcl_file)
      (write-line "                label = \"회전된 치수 (DIMLINEAR)\";" dcl_file)
      (write-line "            }" dcl_file)
      (write-line "            : radio_button {" dcl_file)
      (write-line "                key = \"dim_aligned\";" dcl_file)
      (write-line "                label = \"정렬된 치수 (DIMALIGNED)\";" dcl_file)
      (write-line "            }" dcl_file)
      (write-line "        }" dcl_file)
      (write-line "        " dcl_file)
      (write-line "        // 전체 축척" dcl_file)
      (write-line "        : row {" dcl_file)
      (write-line "            : text {" dcl_file)
      (write-line "                label = \"전체 축척 (DIMSCALE):\";" dcl_file)
      (write-line "                width = 25;" dcl_file)
      (write-line "                alignment = left;" dcl_file)
      (write-line "            }" dcl_file)
      (write-line "            : edit_box {" dcl_file)
      (write-line "                key = \"dimscale\";" dcl_file)
      (write-line "                edit_width = 10;" dcl_file)
      (write-line "                value = \"20\";" dcl_file)
      (write-line "            }" dcl_file)
      (write-line "        }" dcl_file)
      (write-line "        " dcl_file)
      (write-line "        // 문자 높이 (자동 계산)" dcl_file)
      (write-line "        : row {" dcl_file)
      (write-line "            : text {" dcl_file)
      (write-line "                label = \"문자 높이 (DIMTXT):\";" dcl_file)
      (write-line "                width = 25;" dcl_file)
      (write-line "                alignment = left;" dcl_file)
      (write-line "            }" dcl_file)
      (write-line "            : text {" dcl_file)
      (write-line "                key = \"textheight\";" dcl_file)
      (write-line "                width = 10;" dcl_file)
      (write-line "                alignment = left;" dcl_file)
      (write-line "            }" dcl_file)
      (write-line "        }" dcl_file)
      (write-line "        " dcl_file)
      (write-line "        // 화살표 크기 (자동 계산)" dcl_file)
      (write-line "        : row {" dcl_file)
      (write-line "            : text {" dcl_file)
      (write-line "                label = \"화살표 크기 (DIMASZ):\";" dcl_file)
      (write-line "                width = 25;" dcl_file)
      (write-line "                alignment = left;" dcl_file)
      (write-line "            }" dcl_file)
      (write-line "            : text {" dcl_file)
      (write-line "                key = \"arrowsize\";" dcl_file)
      (write-line "                width = 10;" dcl_file)
      (write-line "                alignment = left;" dcl_file)
      (write-line "            }" dcl_file)
      (write-line "        }" dcl_file)
      (write-line "    }" dcl_file)
      (write-line "    " dcl_file)
      (write-line "    : boxed_column {" dcl_file)
      (write-line "        label = \"치수보조선 설정\";" dcl_file)
      (write-line "        " dcl_file)
      (write-line "        // 치수보조선 간격띄우기 (자동 계산)" dcl_file)
      (write-line "        : row {" dcl_file)
      (write-line "            : text {" dcl_file)
      (write-line "                label = \"간격띄우기 (DIMEXO):\";" dcl_file)
      (write-line "                width = 25;" dcl_file)
      (write-line "                alignment = left;" dcl_file)
      (write-line "            }" dcl_file)
      (write-line "            : text {" dcl_file)
      (write-line "                key = \"extoffset\";" dcl_file)
      (write-line "                width = 10;" dcl_file)
      (write-line "                alignment = left;" dcl_file)
      (write-line "            }" dcl_file)
      (write-line "        }" dcl_file)
      (write-line "        " dcl_file)
      (write-line "        // 치수보조선 연장 (자동 계산)" dcl_file)
      (write-line "        : row {" dcl_file)
      (write-line "            : text {" dcl_file)
      (write-line "                label = \"보조선 연장 (DIMEXE):\";" dcl_file)
      (write-line "                width = 25;" dcl_file)
      (write-line "                alignment = left;" dcl_file)
      (write-line "            }" dcl_file)
      (write-line "            : text {" dcl_file)
      (write-line "                key = \"extextend\";" dcl_file)
      (write-line "                width = 10;" dcl_file)
      (write-line "                alignment = left;" dcl_file)
      (write-line "            }" dcl_file)
      (write-line "        }" dcl_file)
      (write-line "    }" dcl_file)
      (write-line "    " dcl_file)
      (write-line "    : boxed_column {" dcl_file)
      (write-line "        label = \"문자 설정\";" dcl_file)
      (write-line "        " dcl_file)
      (write-line "        // 문자와 치수선 간격 (자동 계산)" dcl_file)
      (write-line "        : row {" dcl_file)
      (write-line "            : text {" dcl_file)
      (write-line "                label = \"문자 간격 (DIMGAP):\";" dcl_file)
      (write-line "                width = 25;" dcl_file)
      (write-line "                alignment = left;" dcl_file)
      (write-line "            }" dcl_file)
      (write-line "            : text {" dcl_file)
      (write-line "                key = \"textgap\";" dcl_file)
      (write-line "                width = 10;" dcl_file)
      (write-line "                alignment = left;" dcl_file)
      (write-line "            }" dcl_file)
      (write-line "        }" dcl_file)
      (write-line "    }" dcl_file)
      (write-line "    " dcl_file)
      (write-line "    spacer;" dcl_file)
      (write-line "    " dcl_file)
      (write-line "    // 버튼" dcl_file)
      (write-line "    : row {" dcl_file)
      (write-line "        : spacer { width = 1; }" dcl_file)
      (write-line "        " dcl_file)
      (write-line "        : button {" dcl_file)
      (write-line "            key = \"accept\";" dcl_file)
      (write-line "            label = \"확인\";" dcl_file)
      (write-line "            is_default = true;" dcl_file)
      (write-line "            fixed_width = true;" dcl_file)
      (write-line "            width = 12;" dcl_file)
      (write-line "        }" dcl_file)
      (write-line "        " dcl_file)
      (write-line "        : button {" dcl_file)
      (write-line "            key = \"cancel\";" dcl_file)
      (write-line "            label = \"취소\";" dcl_file)
      (write-line "            is_cancel = true;" dcl_file)
      (write-line "            fixed_width = true;" dcl_file)
      (write-line "            width = 12;" dcl_file)
      (write-line "        }" dcl_file)
      (write-line "        " dcl_file)
      (write-line "        : spacer { width = 1; }" dcl_file)
      (write-line "    }" dcl_file)
      (write-line "}" dcl_file)
      
      (close dcl_file)
      (princ (strcat "\nDCL 파일 생성됨: " dcl_path))
      dcl_path ; DCL 파일 경로 반환
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
(defun C:DM (/ dcl_id result dimstyle-name dcl_path base_scale)
  (princ "\n치수 스타일 생성 프로그램 시작...")
  
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
  (setq *dim_scale* *last_dim_scale*)  ; 마지막 사용 축척
  (setq *dim_type* *last_dim_type*)    ; 마지막 사용 치수 타입
  
  ;; ISO-25 표준 기준값 (축척 1:20 기준)
  (setq base_scale 20.0)           ; 기준 축척
  (setq base_text_height 3.0)      ; 기준 문자 높이
  (setq base_arrow_size 2.5)       ; 기준 화살표 크기
  (setq base_ext_offset 10.0)      ; 기준 치수보조선 간격
  (setq base_ext_extend 1.25)      ; 기준 치수보조선 연장
  (setq base_text_gap 0.625)       ; 기준 문자 간격
  
  ;; DCL 컨트롤 초기화
  (set_tile "dimscale" *dim_scale*)
  
  ;; 치수 타입 라디오 버튼 초기화
  (if (= *dim_type* "0")
    (set_tile "dim_linear" "1")   ; 회전된 치수 선택
    (set_tile "dim_aligned" "1")  ; 정렬된 치수 선택
  )
  
  ;; 계산된 값들을 표시 (읽기 전용)
  (set_tile "textheight" (rtos base_text_height 2 2))
  (set_tile "arrowsize" (rtos base_arrow_size 2 2))
  (set_tile "extoffset" (rtos base_ext_offset 2 2))
  (set_tile "extextend" (rtos base_ext_extend 2 2))
  (set_tile "textgap" (rtos base_text_gap 2 3))
  
  ;; 액션 설정 - 치수 타입 선택
  (action_tile "dim_linear" "(setq *dim_type* \"0\")")
  (action_tile "dim_aligned" "(setq *dim_type* \"1\")")
  
  ;; 액션 설정 - 전체 축척 변경 시 모든 값 재계산
  (action_tile "dimscale" 
    "(progn
       (setq *dim_scale* $value)
       (setq scale_ratio (/ (atof *dim_scale*) base_scale))
       (set_tile \"textheight\" (rtos (* base_text_height scale_ratio) 2 2))
       (set_tile \"arrowsize\" (rtos (* base_arrow_size scale_ratio) 2 2))
       (set_tile \"extoffset\" (rtos (* base_ext_offset scale_ratio) 2 2))
       (set_tile \"extextend\" (rtos (* base_ext_extend scale_ratio) 2 2))
       (set_tile \"textgap\" (rtos (* base_text_gap scale_ratio) 2 3))
     )"
  )
  
  (action_tile "accept" "(done_dialog 1)")
  (action_tile "cancel" "(done_dialog 0)")
  
  ;; 대화상자 표시
  (setq result (start_dialog))
  
  (unload_dialog dcl_id)
  
  ;; (wcmatch "비교문자열" "패턴") - 와일드카드 비교
  (if (not (wcmatch (strcase (vl-filename-base dcl_path)) "DIMSTYLE"))
    (if (findfile dcl_path)
      (progn
        (vl-file-delete dcl_path)
        (princ (strcat "\n임시 DCL 파일 삭제됨: " dcl_path))
      )
    )
    (princ (strcat "\nDCL 파일 유지됨: " dcl_path))
  )
  
  ;; 사용자가 확인을 누른 경우
  (if (= result 1)
    (progn
      (princ "\n치수 스타일 생성 중...")
      
      ;; '!-치수' 레이어 확인 및 생성 (빨간색: 1)
      (ensure_layer "!-치수" 1)
      
      ;; 현재 레이어를 '!-치수'로 변경
      (setvar "CLAYER" "!-치수")
      (princ "\n현재 레이어: !-치수")
      
      ;; 마지막 사용 축척 및 타입 저장
      (setq *last_dim_scale* *dim_scale*)
      (setq *last_dim_type* *dim_type*)
      
      ;; 비율에 따라 모든 값 계산
      (setq scale_ratio (/ (atof *dim_scale*) base_scale))
      (setq *dim_text_height* (rtos (* base_text_height scale_ratio) 2 2))
      (setq *dim_arrow_size* (rtos (* base_arrow_size scale_ratio) 2 2))
      (setq *dim_ext_offset* (rtos (* base_ext_offset scale_ratio) 2 2))
      (setq *dim_ext_extend* (rtos (* base_ext_extend scale_ratio) 2 2))
      (setq *dim_text_gap* (rtos (* base_text_gap scale_ratio) 2 3))
      
      ;; 치수 스타일 생성
      (setq dimstyle-name "ISO-25-Custom")
      (create_dimstyle dimstyle-name)
      
      ;; 연속 치수 그리기 (루프)
      (princ "\n치수 그리기 시작... (ESC로 종료)")
      (princ (strcat "\n현재 치수 스타일: " dimstyle-name))
      (princ (strcat "\n전체 축척: " *dim_scale*))
      
      ;; 연속 치수 그리기 루프
      (setq continue_loop T)
      (while continue_loop
        ;; 치수 그리기 (세 점 입력: pt1, pt2, pt3-방향)
        (setq pt1 (getpoint "\n첫 번째 치수보조선 원점 또는 [ESC 종료]: "))
        
        (if pt1
          (progn
            (setq pt2 (getpoint pt1 "\n두 번째 치수보조선 원점: "))
            
            (if pt2
              (progn
                ;; pt3: 치수선 방향 지정 (DIM 명령어의 마우스 방향과 동일)
                (setq pt3 (getpoint pt2 "\n치수선 방향 지정: "))
                
                (if pt3
                  (progn
                    ;; pt1과 pt2의 중간점 계산
                    (setq mid_x (/ (+ (car pt1) (car pt2)) 2.0))
                    (setq mid_y (/ (+ (cadr pt1) (cadr pt2)) 2.0))
                    (setq mid_pt (list mid_x mid_y))
                    
                    ;; 중간점에서 pt3으로 향하는 방향 벡터 계산
                    (setq dir_x (- (car pt3) mid_x))
                    (setq dir_y (- (cadr pt3) mid_y))
                    (setq dir_dist (sqrt (+ (* dir_x dir_x) (* dir_y dir_y))))
                    
                    ;; 방향 벡터를 단위 벡터로 정규화
                    (if (> dir_dist 0.001)
                      (progn
                        (setq unit_dir_x (/ dir_x dir_dist))
                        (setq unit_dir_y (/ dir_y dir_dist))
                        
                        ;; 중간점에서 pt3 방향으로 300 단위 떨어진 위치 계산
                        (setq dim_pt (list 
                          (+ mid_x (* unit_dir_x 300.0))
                          (+ mid_y (* unit_dir_y 300.0))
                        ))
                        
                        ;; 선택된 치수 타입에 따라 명령어 실행
                        (if (= *dim_type* "0")
                          (progn
                            ;; 회전된 치수 (DIMLINEAR)
                            (command "._DIMLINEAR" pt1 pt2 dim_pt)
                            (princ "\n회전된 치수 생성 완료. 다음 치수를 계속 그립니다...")
                          )
                          (progn
                            ;; 정렬된 치수 (DIMALIGNED)
                            (command "._DIMALIGNED" pt1 pt2 dim_pt)
                            (princ "\n정렬된 치수 생성 완료. 다음 치수를 계속 그립니다...")
                          )
                        )
                      )
                      (princ "\n방향 지정 지점이 중간점과 너무 가깝습니다.")
                    )
                  )
                  ;; pt3가 nil이면 사용자가 ESC를 누름
                  (progn
                    (princ "\n치수 그리기 취소.")
                    (setq continue_loop nil)
                  )
                )
              )
              ;; pt2가 nil이면 사용자가 ESC를 누름
              (progn
                (princ "\n치수 그리기 종료.")
                (setq continue_loop nil)
              )
            )
          )
          ;; pt1이 nil이면 사용자가 ESC를 누르거나 Enter를 누름
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
;;;; 레이어 생성/확인 함수
;;;; ============================================================================
(defun ensure_layer (layer-name layer-color / layer-table layer-obj)
  ;; 레이어가 존재하는지 확인
  (if (not (tblsearch "LAYER" layer-name))
    (progn
      ;; 레이어가 없으면 새로 생성
      (command "._-LAYER" "_N" layer-name "_C" layer-color layer-name "")
      (princ (strcat "\n레이어 '" layer-name "' 생성됨 (색상: " (itoa layer-color) ")"))
    )
    (princ (strcat "\n레이어 '" layer-name "' 이미 존재함"))
  )
)

;;;; ============================================================================
;;;; 치수 스타일 생성 함수 - ISO-25 전체 설정
;;;; ============================================================================
(defun create_dimstyle (style-name /)
  
  ;; ========================================
  ;; [1] 선(Line) 탭 설정
  ;; ========================================
  
  ;; 치수선 설정
  (setvar "DIMCLRD" 256)                 ; 치수선 색상: ByLayer
  (setvar "DIMDLE" 0.0)                  ; 눈금 너머로 연장: 0
  (setvar "DIMDLI" 3.75)                 ; 기준선 간격: 3.75
  
  ;; 치수보조선 설정
  (setvar "DIMCLRE" 256)                 ; 치수보조선 색상: ByLayer
  (setvar "DIMEXE" (atof *dim_ext_extend*))   ; 치수선 너머로 연장: 1.25
  (setvar "DIMEXO" (atof *dim_ext_offset*))   ; 원점에서 간격띄우기: 0.625
  (setvar "DIMSE1" 0)                    ; 첫번째 치수보조선 표시
  (setvar "DIMSE2" 0)                    ; 두번째 치수보조선 표시
  
  ;; ========================================
  ;; [2] 화살표(Symbols) 탭 설정
  ;; ========================================
  
  ;; 화살촉 설정
  (setvar "DIMBLK" "")                    ; 첫 번째: 닫힌 채움 (기본값 = 빈 문자열)
  (setvar "DIMBLK1" "")                   ; 두 번째: 닫힌 채움
  (setvar "DIMBLK2" "")                   ; 지시선: 닫힌 채움
  (setvar "DIMASZ" (atof *dim_arrow_size*)) ; 화살표 크기: 2.5
  
  ;; 중심 표시
  (setvar "DIMCEN" 2.5)                  ; 중심 표시 크기: 2.5 (양수 = 표식)
  
  ;; ========================================
  ;; [3] 문자(Text) 탭 설정
  ;; ========================================
  
  ;; 문자 모양
  (setvar "DIMTXSTY" "Standard")         ; 문자 스타일: Standard
  (setvar "DIMCLRT" 7)                   ; 문자 색상: 흰색 (7)
  (setvar "DIMTXT" (atof *dim_text_height*)) ; 문자 높이: 3
  (setvar "DIMTFAC" 1.0)                 ; 분수 높이 축척: 1
  
  ;; 문자 배치
  (setvar "DIMTAD" 1)                    ; 수직: 위 (1)
  (setvar "DIMJUST" 0)                   ; 수평: 중심 (0)
  (setvar "DIMGAP" (atof *dim_text_gap*))  ; 치수선에 관계없이: 0.625
  
  ;; 문자 정렬
  (setvar "DIMTIH" 0)                    ; 치수선에 정렬 (내부 수평: 끄기)
  (setvar "DIMTOH" 0)                    ; 치수선에 정렬 (외부 수평: 끄기)
  
  ;; ========================================
  ;; [4] 맞춤(Fit) 탭 설정
  ;; ========================================
  
  ;; 맞춤 옵션
  (setvar "DIMATFIT" 2)                 ; 화살표를 먼저 이동 (2)
  
  ;; 문자 배치
  (setvar "DIMTMOVE" 1)                 ; 문자가 기본 위치에 없을 경우: 치수선 위에 지시선 추가 (1)
  
  ;; 치수 축척
  (setvar "DIMSCALE" (atof *dim_scale*))   ; 전체 축척: 20
  
  ;; 최소으로 조정
  (setvar "DIMTOFL" 1)                   ; 치수보조선 사이에 치수선 그리기: 1 (켜짐)
  
  ;; ========================================
  ;; [5] 1차 단위(Primary Units) 탭 설정
  ;; ========================================
  
  ;; 선형 치수
  (setvar "DIMLUNIT" 6)                 ; 단위 형식: 십진법 + 천 단위 구분 기호 (6)
  (setvar "DIMDEC" 2)                   ; 정밀도: 0.00 (소수점 2자리)
  
  ;; [수정] 소수 구분 기호 (정수 0이 아닌 문자열 "."로 설정)
  ;; (ascii ".")는 46을 반환하여 오류 발생.
  ;; setvar로 DIMDSEP를 설정할 때는 예외적으로 문자열을 사용해야 합니다.
  (setvar "DIMDSEP" ".")                   ; 소수 구분 기호: . (점)
  
  (setvar "DIMRND" 0)                    ; 반올림: 0
  
  ;; 측정 축척
  (setvar "DIMLFAC" 1.0)                 ; 비율: 1
  
  ;; [수정] 0 억제
  ;; 12 (8+4)는 선행 0 (.5)과 후행 0 (12.5)을 모두 억제합니다.
  ;; ISO 표준은 선행 0을 표시(0.5)하므로, 후행 0만 억제하는 8을 사용합니다.
  (setvar "DIMZIN" 8)                    ; 후행 0 억제 (8)
  
  ;; 각도 치수
  (setvar "DIMAUNIT" 0)                 ; 단위 형식: 십진 도수 (0)
  (setvar "DIMADEC" 0)                  ; 정밀도: 0
  (setvar "DIMAZIN" 0)                  ; 각도 0 억제: 없음
  
  ;; ========================================
  ;; [6] 대체 단위(Alternate Units) 탭 설정
  ;; ========================================
  
  ;; 대체 단위 표시
  (setvar "DIMALT" 0)                    ; 대체 단위 표시: 끄기 (0)
  (setvar "DIMALTF" 0.03937007)          ; 대체 단위 승수: mm to inch
  (setvar "DIMALTD" 3)                   ; 대체 단위 정밀도: 0.000
  (setvar "DIMALTRND" 0)                 ; 반올림: 0
  
  ;; ========================================
  ;; [7] 공차(Tolerances) 탭 설정
  ;; ========================================
  
  ;; 공차 형식
  (setvar "DIMTOL" 0)                    ; 공차 표시: 없음 (0)
  (setvar "DIMTP" 0)                     ; 상한값: 0
  (setvar "DIMTM" 0)                     ; 하한값: 0
  (setvar "DIMTFAC" 1.0)                 ; 높이에 대한 축척: 1
  (setvar "DIMTOLJ" 0)                   ; 수직 위치: 맨 아래 (0)
  
  ;; 대체 단위 공차
  (setvar "DIMALTD" 3)                   ; 정밀도: 0.000
  
  ;; ========================================
  ;; 치수 스타일 저장 및 적용
  ;; ========================================
  
  ;; [수정] (setvar)로 변수만 설정하면 현재 스타일에만 적용됩니다.
  ;; (command)를 사용하기 전에 (setvar)를 사용하면
  ;; -DIMSTYLE -> S(저장) 명령이 현재 설정된 변수값을 
  ;; "style-name"으로 저장하게 됩니다.
  
  ;; 치수 스타일이 이미 있는지 확인
  (if (tblsearch "DIMSTYLE" style-name)
    (progn
      ;; 있으면 덮어쓰기
      (command "._-DIMSTYLE" "_S" style-name "_Y")
    )
    (progn
      ;; 없으면 새로 생성 (현재 설정값 기준)
      (command "._-DIMSTYLE" "_S" style-name)
    )
  )
  
  ;; 새로 만든 스타일을 현재 스타일로 설정
  (command "._-DIMSTYLE" "_R" style-name)
  
  (princ (strcat "\n치수 스타일 '" style-name "' 생성 완료!"))
  (princ "\n=== ISO-25 표준 설정 적용됨 ===")
  (princ (strcat "\n  전체 축척: " *dim_scale*))
  (princ (strcat "\n  문자 높이: " *dim_text_height*))
  (princ (strcat "\n  화살표 크기: " *dim_arrow_size*))
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
