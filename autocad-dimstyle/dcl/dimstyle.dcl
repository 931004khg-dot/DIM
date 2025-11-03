// ============================================================================
// AutoCAD 치수 스타일 DCL 대화상자
// 파일명: dimstyle.dcl
// 설명: 치수 스타일 옵션을 조정하기 위한 대화상자
// ============================================================================

dimstyle : dialog {
    label = "ISO-25 치수 스타일 설정";
    
    : boxed_column {
        label = "기본 설정 (ISO-25 표준)";
        
        // 전체 축척
        : row {
            : text {
                label = "전체 축척 (DIMSCALE):";
                width = 25;
                alignment = left;
            }
            : edit_box {
                key = "dimscale";
                edit_width = 10;
                value = "20";
            }
        }
        
        // 문자 높이
        : row {
            : text {
                label = "문자 높이 (DIMTXT):";
                width = 25;
                alignment = left;
            }
            : edit_box {
                key = "textheight";
                edit_width = 10;
                value = "3";
            }
        }
        
        // 화살표 크기
        : row {
            : text {
                label = "화살표 크기 (DIMASZ):";
                width = 25;
                alignment = left;
            }
            : edit_box {
                key = "arrowsize";
                edit_width = 10;
                value = "2.5";
            }
        }
    }
    
    : boxed_column {
        label = "치수보조선 설정";
        
        // 치수보조선 간격띄우기
        : row {
            : text {
                label = "간격띄우기 (DIMEXO):";
                width = 25;
                alignment = left;
            }
            : edit_box {
                key = "extoffset";
                edit_width = 10;
                value = "0.625";
            }
        }
        
        // 치수보조선 연장
        : row {
            : text {
                label = "보조선 연장 (DIMEXE):";
                width = 25;
                alignment = left;
            }
            : edit_box {
                key = "extextend";
                edit_width = 10;
                value = "1.25";
            }
        }
    }
    
    : boxed_column {
        label = "문자 설정";
        
        // 문자와 치수선 간격
        : row {
            : text {
                label = "문자 간격 (DIMGAP):";
                width = 25;
                alignment = left;
            }
            : edit_box {
                key = "textgap";
                edit_width = 10;
                value = "0.625";
            }
        }
    }
    
    spacer;
    
    // 버튼
    : row {
        : spacer { width = 1; }
        
        : button {
            key = "accept";
            label = "확인";
            is_default = true;
            fixed_width = true;
            width = 12;
        }
        
        : button {
            key = "cancel";
            label = "취소";
            is_cancel = true;
            fixed_width = true;
            width = 12;
        }
        
        : spacer { width = 1; }
    }
}

// ============================================================================
// 간단한 정보 대화상자
// ============================================================================

dimstyle_info : dialog {
    label = "치수 스타일 정보";
    
    : text {
        label = "ISO-25 기반 치수 스타일이 생성되었습니다.";
        alignment = centered;
    }
    
    spacer;
    
    : text {
        label = "치수 명령어를 사용하여 치수를 그릴 수 있습니다.";
        alignment = centered;
    }
    
    spacer;
    
    : button {
        key = "accept";
        label = "확인";
        is_default = true;
        fixed_width = true;
        width = 12;
        alignment = centered;
    }
}
