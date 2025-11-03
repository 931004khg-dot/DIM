# AutoCAD 치수 스타일 LISP 프로그램

ISO-25 기반 치수 스타일을 자동으로 생성하고 관리하는 AutoCAD LISP 프로그램입니다.

![License](https://img.shields.io/badge/license-MIT-blue.svg)
![AutoCAD](https://img.shields.io/badge/AutoCAD-2000%2B-red.svg)
![Language](https://img.shields.io/badge/language-AutoLISP-green.svg)

## 📋 프로젝트 개요

AutoCAD에서 치수 스타일(Dimension Style)은 기본적으로 개별 도면 파일(.dwg)에 저장됩니다. 이 프로그램은 LISP 파일로 치수 스타일을 정의하여, 어떤 도면 파일에서도 일관된 치수 스타일을 사용할 수 있도록 합니다.

### 주요 특징

- ✅ **ISO-25 규격 기반** 치수 스타일 자동 생성
- ✅ **DCL 대화상자**로 직관적인 옵션 조정
- ✅ **템플릿 파일 불필요** - LISP만으로 모든 기능 제공
- ✅ **파일 독립적** - 어떤 DWG 파일에서도 사용 가능
- ✅ **커스터마이징 가능** - 필요에 따라 설정 조정
- ✅ **DCL 파일 자동 생성** - LISP 파일 하나만 배포하면 됨
- ✅ **ANSI 인코딩 호환** - 유니코드 문제 없음

## 📁 프로젝트 구조

```
autocad-dimstyle/
├── lisp/
│   └── dimstyle.lsp          # 메인 LISP 프로그램
├── dcl/
│   └── dimstyle.dcl          # DCL 대화상자 정의
├── docs/
│   └── manual.md             # 상세 사용 설명서
├── examples/
│   └── example-usage.lsp     # 사용 예제 및 보조 명령어
└── README.md                 # 이 파일
```

## 🚀 빠른 시작

### 1. 파일 다운로드

`lisp/dimstyle.lsp` 파일만 다운로드하세요!

**✨ 새 기능**: DCL 파일은 LISP 실행 시 자동으로 생성됩니다!

### 2. AutoCAD에 로드

#### 방법 A: APPLOAD 사용 (권장)
```
명령어: APPLOAD
→ dimstyle.lsp 선택
→ Load 버튼 클릭
```

#### 방법 B: 드래그 앤 드롭
- `dimstyle.lsp` 파일을 AutoCAD 창으로 드래그 앤 드롭

#### 방법 C: 명령줄
```lisp
(load "C:/경로/dimstyle.lsp")
```

### 3. 사용

```
명령어: MYDIM
```

DCL 대화상자가 나타나면 옵션을 조정하고 "확인"을 클릭하세요!

## 📖 사용 방법

### 기본 사용법

1. **명령어 실행**
   ```
   명령어: MYDIM
   ```

2. **옵션 조정**
   - DCL 대화상자에서 치수 스타일 옵션 설정
   - 기본값은 ISO-25 규격 기준

3. **치수 그리기**
   - 확인 버튼 클릭 후 자동으로 치수 명령 실행
   - 또는 수동으로 치수 명령어 사용:
     - `DIMLINEAR` - 선형 치수
     - `DIMALIGNED` - 정렬 치수
     - `DIMRADIUS` - 반지름 치수
     - `DIMDIAMETER` - 직경 치수

### DCL 대화상자 옵션

| 옵션 | 기본값 | 설명 |
|------|--------|------|
| **전체 축척** | 20 | 모든 치수 요소의 크기 조절 (DIMSCALE) |
| **문자 높이** | 3 | 치수 문자 높이 (DIMTXT) |
| **화살표 크기** | 2.5 | 치수선 끝 화살표 크기 (DIMASZ) |
| **간격띄우기** | 0.625 | 객체와 치수보조선 간격 (DIMEXO) |
| **보조선 연장** | 1.25 | 치수선 넘어 연장되는 길이 (DIMEXE) |
| **문자 간격** | 0.625 | 문자와 치수선 간격 (DIMGAP) |

## 📚 상세 문서

더 자세한 정보는 다음 문서를 참조하세요:

- [📘 사용 설명서](docs/manual.md) - 상세한 설치 및 사용 가이드
- [💡 예제 파일](examples/example-usage.lsp) - 추가 명령어 및 사용 예제

## 🛠️ 추가 기능 (예제 파일)

`example-usage.lsp` 파일에는 다음과 같은 추가 명령어가 포함되어 있습니다:

### 정보 명령어
- `DIMINFO` - 현재 치수 스타일 정보 출력
- `DIMCOMPARE` - 여러 치수 스타일 비교

### 자동 생성 명령어
- `MAKEDIM` - 대화상자 없이 자동으로 치수 스타일 생성
- `MAKEMULTIDIM` - ISO-25, ISO-50, ISO-100 스타일 일괄 생성

### 빠른 치수 명령어
- `DL` - 선형 치수 (DIMLINEAR)
- `DA` - 정렬 치수 (DIMALIGNED)
- `DR` - 반지름 치수 (DIMRADIUS)
- `DD` - 직경 치수 (DIMDIAMETER)
- `DAN` - 각도 치수 (DIMANGULAR)

## 🔧 커스터마이징

### 기본값 변경

`dimstyle.lsp` 파일의 다음 부분을 수정하세요:

```lisp
;; 기본값 설정
(setq *dim_scale* "25")          ; 전체 축척
(setq *dim_text_height* "2.5")   ; 문자 높이
(setq *dim_arrow_size* "2.5")    ; 화살표 크기
(setq *dim_ext_offset* "0.625")  ; 치수보조선 간격띄우기
(setq *dim_ext_extend* "1.25")   ; 치수보조선 연장
(setq *dim_text_gap* "0.625")    ; 문자와 치수선 간격
```

### 추가 설정

`create_dimstyle` 함수에서 더 많은 치수 변수를 설정할 수 있습니다:

```lisp
(setvar "DIMCLRD" 1)    ; 치수선 색상 (1=빨강)
(setvar "DIMCLRE" 2)    ; 치수보조선 색상 (2=노랑)
(setvar "DIMCLRT" 3)    ; 문자 색상 (3=초록)
(setvar "DIMDEC" 1)     ; 소수점 자릿수 (1자리)
```

## 💾 자동 로드 설정

AutoCAD 시작 시 자동으로 로드하려면:

### 방법 1: Startup Suite
1. `APPLOAD` 명령 실행
2. `Contents` 버튼 (Startup Suite)
3. `Add` 버튼으로 `dimstyle.lsp` 추가

### 방법 2: acaddoc.lsp
AutoCAD 지원 폴더에 `acaddoc.lsp` 파일 생성:

```lisp
(load "C:/경로/dimstyle.lsp")
(princ "\n치수 스타일 LISP 자동 로드됨!")
```

## 🐛 문제 해결

### DCL 파일을 찾을 수 없음
- `dimstyle.lsp`와 `dimstyle.dcl`이 같은 폴더에 있는지 확인
- AutoCAD 지원 파일 경로에 해당 폴더 추가
  - `OPTIONS` → `Files` → `Support File Search Path`

### 치수 스타일이 적용되지 않음
- 명령 프롬프트에서 오류 메시지 확인
- `DIMSTYLE` 명령으로 "ISO-25-Custom" 스타일 존재 확인
- AutoCAD 버전 확인 (2000 이상 필요)

### 값이 반영되지 않음
- 숫자 값만 입력 (문자 입력 불가)
- 양수 값 사용
- "확인" 버튼 클릭 확인

## 📊 ISO-25 규격 설정값

| 설정 항목 | 값 | AutoCAD 변수 |
|-----------|-----|---------------|
| 전체 축척 | 25 | DIMSCALE |
| 문자 높이 | 2.5 | DIMTXT |
| 문자 위치 | 치수선 위 | DIMTAD = 1 |
| 문자 방향 | 항상 수평 | DIMTIH = 0, DIMTOH = 0 |
| 화살표 크기 | 2.5 | DIMASZ |
| 치수보조선 간격 | 0.625 | DIMEXO |
| 치수보조선 연장 | 1.25 | DIMEXE |
| 치수선 간격 | 10.0 | DIMDLI |
| 단위 | 십진법 | DIMUNIT = 2 |
| 소수점 자릿수 | 2 | DIMDEC = 2 |

## 🔄 호환성

- **AutoCAD 버전**: 2000 이상
- **언어**: AutoLISP / Visual LISP
- **운영체제**: Windows, Mac (AutoCAD 지원 플랫폼)
- **테스트 버전**: AutoCAD 2020, 2021, 2024

## 📝 향후 개발 계획

- [ ] 다국어 지원 (한국어/영어)
- [ ] 더 많은 ISO 규격 사전 정의 (ISO-50, ISO-100)
- [ ] 치수 스타일 가져오기/내보내기
- [ ] 배치 공간 자동 축척 조정
- [ ] 단위 변환 기능 (mm, inch, m)
- [ ] 문자 스타일 자동 설정
- [ ] 프리셋 관리 기능

## 🤝 기여

이 프로젝트에 대한 개선 제안이나 버그 리포트는 환영합니다!

## 📄 라이선스

MIT License - 자유롭게 사용, 수정, 배포 가능

---

## 📞 지원

문제가 발생하거나 질문이 있으신 경우:

1. [사용 설명서](docs/manual.md) 확인
2. 예제 파일 참조
3. AutoCAD 명령 프롬프트의 오류 메시지 확인

---

**제작일**: 2025-11-03  
**버전**: 1.0  
**제작 목적**: AutoCAD 작업 흐름 자동화 및 효율성 향상
