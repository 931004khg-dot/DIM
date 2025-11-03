# AutoCAD 치수 스타일 LISP 프로그램

AutoCAD에서 사용할 수 있는 ISO-25 기반 치수 스타일 생성 및 적용 LISP 프로그램입니다.

## 프로젝트 개요

이 프로그램은 AutoCAD에서 커스텀 치수 스타일을 리습(LISP) 파일로 저장하여, 어떤 도면 파일(.dwg)에서도 동일한 치수 스타일을 사용할 수 있도록 합니다.

## 주요 기능

- **ISO-25 기반 치수 스타일 자동 생성**
- **DCL 대화상자를 통한 옵션 조정**
- **치수선 직접 그리기 기능**
- **템플릿 파일 불필요** - LISP 파일만으로 치수 스타일 적용 가능

## 프로젝트 구조

```
autocad-dimstyle/
├── lisp/              # LISP 파일들
│   └── dimstyle.lsp   # 메인 LISP 프로그램
├── dcl/               # DCL 대화상자 파일들
│   └── dimstyle.dcl   # 치수 스타일 옵션 대화상자
├── docs/              # 문서
│   └── manual.md      # 사용 설명서
└── examples/          # 예제 파일들
```

## 설치 방법

1. `lisp/dimstyle.lsp` 파일과 `dcl/dimstyle.dcl` 파일을 다운로드
2. 두 파일을 같은 폴더에 저장
3. AutoCAD에서 `APPLOAD` 명령어로 `dimstyle.lsp` 파일 로드

## 사용 방법

1. LISP 파일 로드 후 명령어 입력: `MYDIM`
2. DCL 대화상자에서 옵션 조정
3. 확인 버튼 클릭
4. 치수선 그리기 시작

## 개발 현황

- [x] 프로젝트 구조 생성
- [ ] ISO-25 치수 스타일 설정 구현
- [ ] DCL 대화상자 구현
- [ ] 치수 그리기 기능 구현
- [ ] 문서화

## 요구사항

- AutoCAD 2000 이상 (LISP 및 DCL 지원)
- 기본 ISO-25 규격 지원

## 라이선스

MIT License

## 작성자

Created for AutoCAD workflow automation
