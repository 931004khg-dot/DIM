# 🎯 MLEADER 속성 완전 매칭 가이드

## ✅ 업데이트 완료!

DM.lsp의 `create_mleader_style` 함수가 업데이트되어 **모든 MLEADER 속성이 DIMSTYLE과 동일하게** 설정됩니다.

---

## 📊 DIMSTYLE ↔ MLEADER 속성 매칭표

### 1️⃣ 문자 설정

| DIMSTYLE 변수 | 값 | MLEADER 속성 | 값 | 설명 |
|---------------|-----|--------------|-----|------|
| `DIMTXT` | 3.0 (기본) | `TextHeight` | 3.0 × DIMSCALE | 문자 높이 |
| `DIMCLRT` | 7 (White) | `TextColor` | 7 (White) | 문자 색상 |
| `DIMTXSTY` | "Standard" | `TextStyle` | "Standard" | 문자 스타일 |
| `DIMGAP` | 0.625 | `LandingGap` | 0.625 × DIMSCALE | 착지 간격 (문자 간격) |

### 2️⃣ 화살표 설정

| DIMSTYLE 변수 | 값 | MLEADER 속성 | 값 | 설명 |
|---------------|-----|--------------|-----|------|
| `DIMASZ` | 2.5 (기본) | `ArrowSize` | 2.5 × DIMSCALE | 화살표 크기 |
| `DIMBLK` | "" (기본) | `ArrowSymbol` | 0 (acArrowDefault) | 화살표 모양 |

### 3️⃣ 지시선 설정

| DIMSTYLE 변수 | 값 | MLEADER 속성 | 값 | 설명 |
|---------------|-----|--------------|-----|------|
| `DIMCLRD` | 256 (ByLayer) | `LeaderLineColor` | 256 (ByLayer) | 지시선 색상 |
| N/A | - | `LeaderLineType` | 1 (직선) | 지시선 유형 |
| N/A | - | `LeaderLineWeight` | -1 (ByLayer) | 지시선 선가중치 |
| N/A | - | `DoglegLength` | 0.36 × DIMSCALE | 착지선 길이 |

### 4️⃣ 전체 축척

| DIMSTYLE 변수 | 값 | MLEADER에 미치는 영향 | 설명 |
|---------------|-----|----------------------|------|
| `DIMSCALE` | 20 (기본) | 모든 크기값 × DIMSCALE | 전체 축척 계수 |

---

## 🆕 MLEADER 전용 속성 (DIMSTYLE에 없음)

다음 속성들은 **MLEADER에만 존재**하며, ISO-25 표준에 맞게 기본값으로 설정됩니다:

### 📝 내용 관련

| 속성 | 값 | 설명 |
|------|-----|------|
| `ContentType` | 2 (acMTextContent) | 내용 유형 = 여러줄 텍스트 |
| `TextAttachmentType` | 1 (acAttachmentMiddleOfTop) | 문자 부착 위치 = 상단 중앙 |
| `TextAngleType` | 0 (acHorizontalAngle) | 문자 각도 = 항상 수평 |
| `TextAlignmentType` | 0 (acLeftAlignment) | 문자 정렬 = 왼쪽 |

### 🎯 착지선 관련

| 속성 | 값 | 설명 |
|------|-----|------|
| `EnableLanding` | 1 (True) | 착지선 사용 = 예 |
| `EnableDogleg` | 1 (True) | 착지선 자동 배치 = 예 |
| `DoglegLength` | 0.36 × DIMSCALE | 착지선 길이 (전체 축척의 36%) |

### 📏 세그먼트 관련

| 속성 | 값 | 설명 |
|------|-----|------|
| `MaxLeaderSegmentsPoints` | 2 | 최대 세그먼트 수 = 2 |

---

## 🎨 속성 설명 - MLEADER 전용

### 1. `ContentType` (내용 유형)
```lisp
(vla-put-ContentType new_style 2)  ; 2 = acMTextContent
```
- **0** = acNoneContent (내용 없음)
- **1** = acBlockContent (블록)
- **2** = acMTextContent (여러줄 텍스트) ✅ ISO-25 선택

### 2. `TextAttachmentType` (문자 부착 위치)
```lisp
(vla-put-TextAttachmentType new_style 1)  ; 1 = acAttachmentMiddleOfTop
```
- **0** = acAttachmentTopOfTop (맨 위)
- **1** = acAttachmentMiddleOfTop (상단 중앙) ✅ ISO-25 선택
- **2** = acAttachmentMiddleOfText (텍스트 중앙)
- **3** = acAttachmentMiddleOfBottom (하단 중앙)
- **4** = acAttachmentBottomOfBottom (맨 아래)
- **9** = acAttachmentUnderlineTopLine (상단 밑줄)

### 3. `TextAngleType` (문자 각도)
```lisp
(vla-put-TextAngleType new_style 0)  ; 0 = acHorizontalAngle
```
- **0** = acHorizontalAngle (항상 수평) ✅ ISO-25 선택
- **1** = acAlignWithLastSegment (마지막 세그먼트와 정렬)
- **2** = acAlignWithLeader (지시선과 정렬)

### 4. `TextAlignmentType` (문자 정렬)
```lisp
(vla-put-TextAlignmentType new_style 0)  ; 0 = acLeftAlignment
```
- **0** = acLeftAlignment (왼쪽) ✅ ISO-25 선택
- **1** = acCenterAlignment (중앙)
- **2** = acRightAlignment (오른쪽)

### 5. `EnableLanding` (착지선 사용)
```lisp
(vla-put-EnableLanding new_style 1)  ; 1 = True
```
- **0** = False (착지선 없음)
- **1** = True (착지선 사용) ✅ ISO-25 선택

### 6. `EnableDogleg` (착지선 자동 배치)
```lisp
(vla-put-EnableDogleg new_style 1)  ; 1 = True
```
- **0** = False (수동)
- **1** = True (자동) ✅ ISO-25 선택

### 7. `MaxLeaderSegmentsPoints` (최대 세그먼트 수)
```lisp
(vla-put-MaxLeaderSegmentsPoints new_style 2)
```
- 지시선의 최대 꺾임 횟수
- ISO-25: **2** (간결한 지시선) ✅

---

## 📋 실행 시 출력 예시

DM.lsp를 실행하고 "지시선" 버튼을 클릭하면 다음과 같은 출력을 볼 수 있습니다:

```
=== MLEADER 스타일 'ISO-25-Custom' 생성 디버깅 시작 ===

[1] CMLEADERSTYLE 변수 확인...
    [성공] MLEADER 지원됨

[2] ActiveX 객체 가져오기...
    [성공] acad_obj 획득
    [성공] doc 획득

[3] Dictionaries를 통해 MLEADERSTYLE 접근 시도...
    [성공] ACAD_MLEADERSTYLE Dictionary 획득

[4] Standard 스타일 확인...
    [성공] Standard 스타일 존재

[5] 새 스타일 추가 시도 (AddObject 사용)...
    [성공] 'ISO-25-Custom' 스타일 생성

[6] 속성 설정 시도 (ISO-25 표준)...
    TextHeight = 60.00
    [성공] TextHeight 설정
    TextColor = White (7)
    [성공] TextColor 설정
    TextStyle = Standard
    [성공] TextStyle 설정
    ArrowSize = 50.00
    [성공] ArrowSize 설정
    ArrowSymbol = 기본 화살표
    [성공] ArrowSymbol 설정
    LeaderLineColor = ByLayer (256)
    [성공] LeaderLineColor 설정
    LeaderLineType = 직선 (1)
    [성공] LeaderLineType 설정
    LeaderLineWeight = ByLayer (-1)
    [성공] LeaderLineWeight 설정
    LandingGap = 12.50
    [성공] LandingGap 설정
    DoglegLength = 7.20
    [성공] DoglegLength 설정

    === MLEADER 전용 속성 ===
    ContentType = 텍스트 (2)
    [성공] ContentType 설정
    TextAttachmentType = 중앙 (1)
    [성공] TextAttachmentType 설정
    TextAngleType = 항상 수평 (0)
    [성공] TextAngleType 설정
    TextAlignmentType = 왼쪽 (0)
    [성공] TextAlignmentType 설정
    EnableLanding = 예 (1)
    [성공] EnableLanding 설정
    EnableDogleg = 예 (1)
    [성공] EnableDogleg 설정
    MaxLeaderSegmentsPoints = 2
    [성공] MaxLeaderSegmentsPoints 설정

[7] 현재 MLEADER 스타일로 설정 시도...
    [성공] CMLEADERSTYLE = ISO-25-Custom

=== 디버깅 종료 ===
```

---

## 🎯 변경 사항 요약

### ✅ DIMSTYLE과 매칭된 속성
1. **문자 색상**: White (7)
2. **문자 스타일**: Standard
3. **화살표 모양**: 기본 화살표
4. **지시선 색상**: ByLayer (256)
5. **지시선 유형**: 직선
6. **지시선 선가중치**: ByLayer

### 🆕 추가된 MLEADER 전용 속성
1. **ContentType**: 여러줄 텍스트
2. **TextAttachmentType**: 상단 중앙 부착
3. **TextAngleType**: 항상 수평
4. **TextAlignmentType**: 왼쪽 정렬
5. **EnableLanding**: 착지선 사용
6. **EnableDogleg**: 착지선 자동 배치
7. **MaxLeaderSegmentsPoints**: 최대 2개 세그먼트

---

## 🔧 사용자 정의 옵션

만약 특정 속성을 변경하고 싶다면 DM.lsp Line 793-1050 섹션에서 값을 수정하세요:

### 예: 문자 부착 위치를 상단 밑줄로 변경
```lisp
;; 변경 전
(vla-put-TextAttachmentType new_style 1)  ; 1 = 상단 중앙

;; 변경 후
(vla-put-TextAttachmentType new_style 9)  ; 9 = 상단 밑줄
```

### 예: 지시선을 스플라인으로 변경
```lisp
;; 변경 전
(vla-put-LeaderLineType new_style 1)  ; 1 = 직선

;; 변경 후
(vla-put-LeaderLineType new_style 2)  ; 2 = 스플라인
```

---

## 📚 참고 자료

### AutoCAD MLEADER 상수값

#### ArrowSymbol 값
- 0 = acArrowDefault (기본 화살표)
- 1 = acArrowDotBlank (빈 점)
- 2 = acArrowArchTick (건축 틱)
- 등등...

#### LeaderLineType 값
- 0 = acInvisibleLeader (보이지 않음)
- 1 = acStraightLeader (직선) ✅
- 2 = acSplineLeader (스플라인)

#### ContentType 값
- 0 = acNoneContent (없음)
- 1 = acBlockContent (블록)
- 2 = acMTextContent (텍스트) ✅

---

## 🎉 완료!

이제 **DM.lsp의 MLEADER 스타일이 DIMSTYLE과 완전히 일치**합니다!

- ✅ 모든 공통 속성 매칭 완료
- ✅ MLEADER 전용 속성 최적화 완료
- ✅ ISO-25 표준 준수
- ✅ 에러 처리 완비
- ✅ 상세한 디버깅 출력

**AutoCAD에서 DM.lsp를 다시 로드하고 테스트해보세요!** 🚀
