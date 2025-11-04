;;;; ============================================================================
;;;; AutoCAD 치수 스타일 생성 LISP 프로그램
;;;; 파일명: dimstyle.lsp
;;;; 설명: ISO-25 기반 치수 스타일을 생성하고 DCL 대화상자로 옵션 조정
;;;; DCL 파일 자동 생성 기능 포함
;;;; ============================================================================

;;;; ============================================================================
;;;; DCL 파일 생성 함수
;;;; ============================================================================
(defun create_dcl_file (/ dcl_file dcl_path)
  ;; LISP 파일과 같은 위치에 DCL 파일 생성
  (setq dcl_path (strcat (getvar "DWGPREFIX") "dimstyle.dcl"))
  
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
      (write-line "        // 문자 높이" dcl_file)
      (write-line "        : row {" dcl_file)
      (write-line "            : text {" dcl_file)
      (write-line "                label = \"문자 높이 (DIMTXT):\";" dcl_file)
      (write-line "                width = 25;" dcl_file)
      (write-line "                alignment = left;" dcl_file)
      (write-line "            }" dcl_file)
      (write-line "            : edit_box {" dcl_file)
      (write-line "                key = \"textheight\";" dcl_file)
      (write-line "                edit_width = 10;" dcl_file)
      (write-line "                value = \"3\";" dcl_file)
      (write-line "            }" dcl_file)
      (write-line "        }" dcl_file)
      (write-line "        " dcl_file)
      (write-line "        // 화살표 크기" dcl_file)
      (write-line "        : row {" dcl_file)
      (write-line "            : text {" dcl_file)
      (write-line "                label = \"화살표 크기 (DIMASZ):\";" dcl_file)
      (write-line "                width = 25;" dcl_file)
      (write-line "                alignment = left;" dcl_file)
      (write-line "            }" dcl_file)
      (write-line "            : edit_box {" dcl_file)
      (write-line "                key = \"arrowsize\";" dcl_file)
      (write-line "                edit_width = 10;" dcl_file)
      (write-line "                value = \"2.5\";" dcl_file)
      (write-line "            }" dcl_file)
      (write-line "        }" dcl_file)
      (write-line "    }" dcl_file)
      (write-line "    " dcl_file)
      (write-line "    : boxed_column {" dcl_file)
      (write-line "        label = \"치수보조선 설정\";" dcl_file)
      (write-line "        " dcl_file)
      (write-line "        // 치수보조선 간격띄우기" dcl_file)
      (write-line "        : row {" dcl_file)
      (write-line "            : text {" dcl_file)
      (write-line "                label = \"간격띄우기 (DIMEXO):\";" dcl_file)
      (write-line "                width = 25;" dcl_file)
      (write-line "                alignment = left;" dcl_file)
      (write-line "            }" dcl_file)
      (write-line "            : edit_box {" dcl_file)
      (write-line "                key = \"extoffset\";" dcl_file)
      (write-line "                edit_width = 10;" dcl_file)
      (write-line "                value = \"0.625\";" dcl_file)
      (write-line "            }" dcl_file)
      (write-line "        }" dcl_file)
      (write-line "        " dcl_file)
      (write-line "        // 치수보조선 연장" dcl_file)
      (write-line "        : row {" dcl_file)
      (write-line "            : text {" dcl_file)
      (write-line "                label = \"보조선 연장 (DIMEXE):\";" dcl_file)
      (write-line "                width = 25;" dcl_file)
      (write-line "                alignment = left;" dcl_file)
      (write-line "            }" dcl_file)
      (write-line "            : edit_box {" dcl_file)
      (write-line "                key = \"extextend\";" dcl_file)
      (write-line "                edit_width = 10;" dcl_file)
      (write-line "                value = \"1.25\";" dcl_file)
      (write-line "            }" dcl_file)
      (write-line "        }" dcl_file)
      (write-line "    }" dcl_file)
      (write-line "    " dcl_file)
      (write-line "    : boxed_column {" dcl_file)
      (write-line "        label = \"문자 설정\";" dcl_file)
      (write-line "        " dcl_file)
      (write-line "        // 문자와 치수선 간격" dcl_file)
      (write-line "        : row {" dcl_file)
      (write-line "            : text {" dcl_file)
      (write-line "                label = \"문자 간격 (DIMGAP):\";" dcl_file)
      (write-line "                width = 25;" dcl_file)
      (write-line "                alignment = left;" dcl_file)
      (write-line "            }" dcl_file)
      (write-line "            : edit_box {" dcl_file)
      (write-line "                key = \"textgap\";" dcl_file)
      (write-line "                edit_width = 10;" dcl_file)
      (write-line "                value = \"0.625\";" dcl_file)
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
(defun C:MYDIM (/ dcl_id result dimstyle-name dcl_path)
  (princ "\n치수 스타일 생성 프로그램 시작...")
  
  ;; DCL 파일 자동 생성
  (setq dcl_path (create_dcl_file))
  
  (if (not dcl_path)
    (progn
      (alert "DCL 파일을 생성할 수 없습니다!")
      (exit)
    )
  )
  
  ;; DCL 파일 로드
  (setq dcl_id (load_dialog dcl_path))
  
  (if (not dcl_id)
    (progn
      (alert "DCL 대화상자를 로드할 수 없습니다!")
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
  
  ;; 기본값 설정 (ISO-25 표준)
  (setq *dim_scale* "20")          ; 치수 전체 축척 (ISO-25 표준: 20)
  (setq *dim_text_height* "3")     ; 문자 높이 (ISO-25 표준: 3)
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
;;;; 치수 스타일 생성 함수 - ISO-25 전체 설정
;;;; ============================================================================
(defun create_dimstyle (style-name /)
  
  ;; ========================================
  ;; [1] 선(Line) 탭 설정
  ;; ========================================
  
  ;; 치수선 설정
  (setvar "DIMCLRD" 256)                             ; 치수선 색상: ByLayer
  (setvar "DIMDLE" 0.0)                              ; 눈금 너머로 연장: 0
  (setvar "DIMDLI" 3.75)                             ; 기준선 간격: 3.75
  
  ;; 치수보조선 설정
  (setvar "DIMCLRE" 256)                             ; 치수보조선 색상: ByLayer
  (setvar "DIMEXE" (atof *dim_ext_extend*))          ; 치수선 너머로 연장: 1.25
  (setvar "DIMEXO" (atof *dim_ext_offset*))          ; 원점에서 간격띄우기: 0.625
  (setvar "DIMSE1" 0)                                ; 첫번째 치수보조선 표시
  (setvar "DIMSE2" 0)                                ; 두번째 치수보조선 표시
  
  ;; ========================================
  ;; [2] 화살표(Symbols) 탭 설정
  ;; ========================================
  
  ;; 화살촉 설정
  (setvar "DIMBLK" "")                               ; 첫 번째: 닫힌 채움 (기본값 = 빈 문자열)
  (setvar "DIMBLK1" "")                              ; 두 번째: 닫힌 채움
  (setvar "DIMBLK2" "")                              ; 지시선: 닫힌 채움
  (setvar "DIMASZ" (atof *dim_arrow_size*))          ; 화살표 크기: 2.5
  
  ;; 중심 표시
  (setvar "DIMCEN" 2.5)                              ; 중심 표시 크기: 2.5 (양수 = 표식)
  
  ;; 참고: DIMJOGANG은 명령어로는 존재하지만 setvar로 설정할 수 없습니다.
  ;; 치수 스타일 대화상자에서 수동으로 설정해야 합니다.
  
  ;; ========================================
  ;; [3] 문자(Text) 탭 설정
  ;; ========================================
  
  ;; 문자 모양
  (setvar "DIMTXSTY" "Standard")                     ; 문자 스타일: Standard
  (setvar "DIMCLRT" 7)                               ; 문자 색상: 흰색 (7)
  (setvar "DIMTXT" (atof *dim_text_height*))         ; 문자 높이: 3
  (setvar "DIMTFAC" 1.0)                             ; 분수 높이 축척: 1
  
  ;; 문자 배치
  (setvar "DIMTAD" 1)                                ; 수직: 위 (1)
  (setvar "DIMJUST" 0)                               ; 수평: 중심 (0)
  (setvar "DIMGAP" (atof *dim_text_gap*))            ; 치수선에 관계없이: 0.625
  
  ;; 문자 정렬
  (setvar "DIMTIH" 0)                                ; 치수선에 정렬 (내부 수평: 끄기)
  (setvar "DIMTOH" 0)                                ; 치수선에 정렬 (외부 수평: 끄기)
  
  ;; ========================================
  ;; [4] 맞춤(Fit) 탭 설정
  ;; ========================================
  
  ;; 맞춤 옵션
  (setvar "DIMATFIT" 3)                              ; 문자 또는 화살표(최대로 맞춤): 3
  
  ;; 문자 배치
  (setvar "DIMTMOVE" 0)                              ; 치수선 외의 배치: 0
  
  ;; 치수 축척
  (setvar "DIMSCALE" (atof *dim_scale*))             ; 전체 축척: 20
  
  ;; 최소으로 조정
  (setvar "DIMTOFL" 1)                               ; 치수보조선 사이에 치수선 그리기: 1 (켜짐)
  
  ;; ========================================
  ;; [5] 1차 단위(Primary Units) 탭 설정
  ;; ========================================
  
  ;; 선형 치수
  (setvar "DIMLUNIT" 2)                              ; 단위 형식: 십진법 (2)
  (setvar "DIMDEC" 2)                                ; 정밀도: 0.00 (소수점 2자리)
  
  ;; 소수 구분 기호 - 버전에 따라 설정 방식이 다름
  ;; AutoCAD 2023에서는 이 변수를 setvar로 설정할 수 없을 수 있음
  ;; (setvar "DIMDSEP" 46)
  
  (setvar "DIMRND" 0)                                ; 반올림: 0
  
  ;; 측정 축척
  (setvar "DIMLFAC" 1.0)                             ; 비율: 1
  
  ;; 0 억제
  (setvar "DIMZIN" 12)                               ; 선행 0 억제 (8 + 4 = 12)
  
  ;; 각도 치수
  (setvar "DIMAUNIT" 0)                              ; 단위 형식: 십진 도수 (0)
  (setvar "DIMADEC" 0)                               ; 정밀도: 0
  (setvar "DIMAZIN" 0)                               ; 각도 0 억제: 없음
  
  ;; ========================================
  ;; [6] 대체 단위(Alternate Units) 탭 설정
  ;; ========================================
  
  ;; 대체 단위 표시
  (setvar "DIMALT" 0)                                ; 대체 단위 표시: 끄기 (0)
  (setvar "DIMALTF" 0.03937007)                      ; 대체 단위 승수: mm to inch
  (setvar "DIMALTD" 3)                               ; 대체 단위 정밀도: 0.000
  (setvar "DIMALTRND" 0)                             ; 반올림: 0
  
  ;; ========================================
  ;; [7] 공차(Tolerances) 탭 설정
  ;; ========================================
  
  ;; 공차 형식
  (setvar "DIMTOL" 0)                                ; 공차 표시: 없음 (0)
  (setvar "DIMTP" 0)                                 ; 상한값: 0
  (setvar "DIMTM" 0)                                 ; 하한값: 0
  (setvar "DIMTFAC" 1.0)                             ; 높이에 대한 축척: 1
  (setvar "DIMTOLJ" 0)                               ; 수직 위치: 맨 아래 (0)
  
  ;; 대체 단위 공차
  (setvar "DIMALTD" 3)                               ; 정밀도: 0.000
  
  ;; ========================================
  ;; 치수 스타일 저장 및 적용
  ;; ========================================
  
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
  (princ "\n=== ISO-25 표준 설정 적용됨 ===")
  (princ (strcat "\n  전체 축척: " *dim_scale*))
  (princ (strcat "\n  문자 높이: " *dim_text_height*))
  (princ (strcat "\n  화살표 크기: " *dim_arrow_size*))
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
(princ "\n  DCL 파일 자동 생성")
(princ "\n========================================")
(princ)
