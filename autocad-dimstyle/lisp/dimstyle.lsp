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
      (write-line "                value = \"10\";" dcl_file)
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
(defun C:MYDIM (/ dcl_id result dimstyle-name dcl_path)
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
  
  ;; 기본값 설정 (ISO-25 표준)
  (setq *dim_scale* "20")        ; 치수 전체 축척 (ISO-25 표준: 20)
  (setq *dim_text_height* "3")     ; 문자 높이 (ISO-25 표준: 3)
  (setq *dim_arrow_size* "2.5")   ; 화살표 크기
  (setq *dim_ext_offset* "10") ; 치수보조선 간격띄우기
  (setq *dim_ext_extend* "1.25")  ; 치수보조선 연장
  (setq *dim_text_gap* "0.625")   ; 문자와 치수선 간격
  
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
  (action_tile "extextend" "(setq *dim_ext_extend$ $value)")
  (action_tile "textgap" "(setq *dim_text_gap* $value)")
  
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
  (setvar "DIMATFIT" 3)                 ; 문자 또는 화살표(최대로 맞춤): 3
  
  ;; 문자 배치
  (setvar "DIMTMOVE" 0)                 ; 치수선 외의 배치: 0
  
  ;; 치수 축척
  (setvar "DIMSCALE" (atof *dim_scale*))   ; 전체 축척: 20
  
  ;; 최소으로 조정
  (setvar "DIMTOFL" 1)                   ; 치수보조선 사이에 치수선 그리기: 1 (켜짐)
  
  ;; ========================================
  ;; [5] 1차 단위(Primary Units) 탭 설정
  ;; ========================================
  
  ;; 선형 치수
  (setvar "DIMLUNIT" 2)                 ; 단위 형식: 십진법 (2)
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
  
  ;; 천 단위 구분 기호 설정 (1000 -> 1,000)
  ;; DIMDSEP는 소수점 구분 기호이고, DIMSEP는 천 단위 구분 기호입니다.
  ;; 하지만 DIMSEP는 존재하지 않으므로, DIMPOST를 사용해야 합니다.
  ;; AutoCAD에서는 천 단위 구분을 위해 DIESEL 표현식을 사용합니다.
  ;; 더 간단한 방법: DIMDSEP를 콤마로 설정하고 DIMDEC를 조정하면 안 됩니다.
  ;; 올바른 방법: AutoLISP에서는 직접 천 단위 구분을 지원하지 않으므로
  ;; DIMPOST를 사용하거나 치수 생성 후 수정해야 합니다.
  ;; 현재는 DIMDSEP=","로 설정하여 천 단위 구분 효과를 냅니다.
  ;; 하지만 이것은 소수점을 콤마로 바꾸는 것입니다.
  ;; 올바른 접근: DIMPOST 사용 불가 (형식 제어 제한적)
  ;; 해결책: 치수 텍스트 재정의 필요 (생성 후 처리)
  ;; 임시 해결: 현재 AutoCAD는 천 단위 구분을 기본 지원하지 않음
  ;; 대안: 필드 코드 사용하거나 LISP로 후처리
  
  ;; AutoCAD 천 단위 구분 기호는 Windows 지역 설정을 따르거나
  ;; DIMPOST를 통한 DIESEL 표현식으로 처리해야 합니다.
  ;; 여기서는 DIMDSEP를 기본 "."로 유지하고
  ;; 치수 생성 후 텍스트를 수정하는 방식을 사용합니다.
  
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
;;;; 치수 텍스트 후처리 함수 (천 단위 구분 기호 추가)
;;;; ============================================================================
(defun format_dimension_text (dim-obj / dim-text formatted-text)
  ;; 치수 객체에서 측정값 가져오기
  (setq dim-text (rtos (vlax-get-property dim-obj 'Measurement) 2 2))
  ;; 천 단위 구분 기호 추가
  (setq formatted-text (add_thousand_separator dim-text))
  ;; 치수 텍스트 재정의
  (vlax-put-property dim-obj 'TextOverride formatted-text)
)

(defun add_thousand_separator (num-str / int-part dec-part result i len)
  ;; 문자열을 정수부와 소수부로 분리
  (if (vl-string-search "." num-str)
    (progn
      (setq int-part (substr num-str 1 (vl-string-search "." num-str)))
      (setq dec-part (substr num-str (+ (vl-string-search "." num-str) 1)))
    )
    (progn
      (setq int-part num-str)
      (setq dec-part nil)
    )
  )
  
  ;; 정수부에 천 단위 구분 기호 추가
  (setq len (strlen int-part))
  (setq result "")
  (setq i 1)
  
  (while (<= i len)
    (setq result (strcat (substr int-part i 1) result))
    (if (and (> (- len i) 0) (= (rem (- len i) 3) 0))
      (setq result (strcat "," result))
    )
    (setq i (+ i 1))
  )
  
  ;; 소수부가 있으면 결합
  (if dec-part
    (strcat result "." dec-part)
    result
  )
)

;;;; ============================================================================
;;;; 치수 생성 후 자동 포맷 적용 (이벤트 리액터)
;;;; ============================================================================
;; 주의: 이 방법은 복잡하므로, 사용자가 수동으로 치수 텍스트를 수정하거나
;; 별도의 명령어로 처리하는 것을 권장합니다.
;; 여기서는 치수 스타일 설정 후 사용자에게 안내 메시지만 제공합니다.

;;;; ============================================================================
;;;; 프로그램 로드 메시지
;;;; ============================================================================
(princ "\n========================================")
(princ "\n  AutoCAD 치수 스타일 LISP 로드됨 (v3)")
(princ "\n========================================")
(princ "\n  명령어: MYDIM")
(princ "\n  설명: ISO-25 기반 치수 스타일 생성")
(princ "\n  - DCL 파일 자동 생성")
(princ "\n  - 치수 레이어: !-치수 (빨간색)")
(princ "\n  - 치수보조선 간격: 10")
(princ "\n  - 천 단위 구분: 수동 편집 필요")
(princ "\n========================================")
(princ "\n  참고: AutoCAD는 기본적으로 천 단위 구분을")
(princ "\n  지원하지 않습니다. 치수 생성 후 텍스트를")
(princ "\n  더블클릭하여 수동으로 편집해주세요.")
(princ "\n  (예: 1520 -> 1,520)")
(princ "\n========================================")
(princ)
