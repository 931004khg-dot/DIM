# DM Command Fixes Summary

## Fixed Issues (2025-11-05)

### 1. MLEADER Style Creation Error ✅

**Problem:**
```
오류: AutoCAD 변수 설정이 거부됨: "CMLEADERSTYLE" "ISO-25-Custom"
```

**Root Cause:**
- `CMLEADERSTYLE` is **not** a settable system variable
- Cannot use `(setvar "CMLEADERSTYLE" style-name)` to create/save MLEADER styles
- MLEADER styles must be created using the `-MLEADERSTYLE` command, similar to dimension styles

**Solution:**
Changed from setvar-based approach to command-based approach:

**Before (Lines 813-857):**
```lisp
(defun create_mleader_style (style-name /)
  ;; INVALID: Cannot set CMLEADERSTYLE via setvar
  (setvar "CMLEADERSTYLE" style-name)
  
  ;; Set other parameters...
  (setvar "CMLEADERTYPE" 1)
  (setvar "CMLEADERCOLOR" 256)
  ;; etc...
  
  ;; No actual style save command!
)
```

**After (Lines 813-874):**
```lisp
(defun create_mleader_style (style-name / old_cmdecho)
  ;; Turn off command echo for clean execution
  (setq old_cmdecho (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  
  ;; Set all MLEADER parameters via setvar
  (setvar "CMLEADERTYPE" 1)
  (setvar "CMLEADERCOLOR" 256)
  (setvar "CMLEADERARROWSIZE" (atof *dim_arrow_size*))
  (setvar "CMTEXTHEIGHT" (atof *dim_text_height*))
  (setvar "CMSCALE" (atof *dim_scale*))
  ;; etc...
  
  ;; Save as MLEADER style using COMMAND (not setvar!)
  (if (tblsearch "MLEADERSTYLE" style-name)
    ;; Style exists - overwrite
    (command "._-MLEADERSTYLE" "_S" style-name "_Y")
    ;; Style doesn't exist - create new
    (command "._-MLEADERSTYLE" "_S" style-name)
  )
  
  ;; Set as current MLEADER style
  (command "._-MLEADERSTYLE" "_R" style-name)
  
  ;; Restore command echo
  (setvar "CMDECHO" old_cmdecho)
)
```

**Key Changes:**
1. **Added CMDECHO control**: Suppress command line noise during execution
2. **Replaced setvar with command**: Use `(command "._-MLEADERSTYLE" "_S" style-name)` to save style
3. **Check for existing style**: Use `_Y` flag to overwrite if style already exists
4. **Set as current**: Use `(command "._-MLEADERSTYLE" "_R" style-name)` to restore/set as current

**AutoCAD Command Syntax:**
```
-MLEADERSTYLE
  _S (Save) - Save current settings as new style
    style-name - Name of the style to save
    _Y - Yes (overwrite if exists)
  _R (Restore) - Set a style as current
    style-name - Name of the style to restore
```

---

### 2. DCL Layout Alignment ✅

**Problem:**
- Scale label width: 20
- Dimension distance label width: 20
- Advanced dialog labels width: 25
- **Inconsistent alignment** across dialog boxes

**Solution:**
Changed both main dialog labels to width 25 for consistency:

**Scale Label (Line 89):**
```lisp
; Before: width = 20;
; After:  width = 25;
```

**Dimension Distance Label (Line 105):**
```lisp
; Before: width = 20;
; After:  width = 25;
```

**Result:**
- All labels now have consistent width of 25
- Proper alignment with advanced dialog settings
- Clean, professional appearance

---

### 3. Leader Button Placement (Question)

**Current Location:**
The "지시선 스타일 생성" (Create Leader Style) button is located at the bottom row with:
- 고급 설정 (Advanced Settings)
- 초기화 (Reset)
- 생성 (Create)
- 취소 (Cancel)

**Alternative Placement Options:**

#### Option 1: Keep Current Position (Recommended)
**Pros:**
- Follows standard dialog button layout patterns
- All action buttons grouped together
- Clear separation between input/output sections
- No structural changes needed

**Cons:**
- Leader button may be overlooked

#### Option 2: Place Near Dimension Type
```
[치수 형식]
( ) 평행 치수 (기본)
( ) 회전 치수
[지시선 스타일 생성]  ← New position
```

**Pros:**
- More visible association with dimension/annotation features
- Easier to discover

**Cons:**
- Breaks visual flow
- Requires DCL restructuring

#### Option 3: Separate Button Group
```
[치수 스타일 관련]
[지시선 스타일 생성]

[고급 설정] [초기화]
[생성] [취소]
```

**Pros:**
- Clear categorization
- Prominent placement

**Cons:**
- Takes more vertical space
- May feel disconnected from main workflow

**Recommendation:** Keep current position (Option 1). The button placement is standard and follows AutoCAD dialog conventions. If needed, we could add a tooltip or change the button label to make it more prominent.

---

## Testing Instructions

1. **Test MLEADER Creation:**
   ```
   Command: DM
   1. Select dimension type
   2. Enter scale (e.g., 20)
   3. Click "지시선 스타일 생성" button
   4. Should create ISO-25-Custom MLEADER style without errors
   ```

2. **Verify MLEADER Style:**
   ```
   Command: -MLEADERSTYLE
   Current: ISO-25-Custom
   
   - Arrow size should match dimension arrow size
   - Text height should match dimension text height
   - Scale should match dimension scale
   ```

3. **Verify DCL Alignment:**
   - Open DM dialog
   - Check that "전체 축척" and "치수선 거리" labels align with other labels
   - All option labels should have consistent left alignment

---

## Technical Notes

### Why setvar vs command?

**setvar (System Variables):**
- Sets temporary session values
- Changes current drawing settings
- Does NOT save to style tables
- Example: `(setvar "DIMSCALE" 20)` - changes current dimension scale

**command (AutoCAD Commands):**
- Executes full AutoCAD commands
- Can save to style tables (DIMSTYLE, MLEADERSTYLE, etc.)
- Required for persistent style creation
- Example: `(command "._-DIMSTYLE" "_S" "MyStyle")` - saves current settings as "MyStyle"

### MLEADER Style System Variables

These variables control **current drawing behavior** but do NOT save to style table:
- `CMLEADERTYPE` - Leader line type (0=spline, 1=straight, 2=none)
- `CMLEADERCOLOR` - Leader line color
- `CMLEADERARROWHEAD` - Arrow block name
- `CMLEADERARROWSIZE` - Arrow size
- `CMTEXTSTYLE` - Text style name
- `CMTEXTHEIGHT` - Text height
- `CMTEXTCOLOR` - Text color
- `CMSCALE` - Overall scale
- `CMLANDING` - Landing line on/off
- `CMLANDINGGAP` - Landing line length

To **save** these settings as a named style, you MUST use:
```lisp
(command "._-MLEADERSTYLE" "_S" "style-name")
```

### Error Prevention Pattern

```lisp
;; Good pattern for style creation:
(defun create_any_style (style-name / old_cmdecho)
  ;; 1. Turn off echo
  (setq old_cmdecho (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  
  ;; 2. Set all parameters via setvar
  (setvar "SOME_PARAMETER" value)
  
  ;; 3. Check if style exists
  (if (tblsearch "STYLE_TABLE" style-name)
    ;; 4a. Overwrite existing
    (command "._-STYLECMD" "_S" style-name "_Y")
    ;; 4b. Create new
    (command "._-STYLECMD" "_S" style-name)
  )
  
  ;; 5. Set as current
  (command "._-STYLECMD" "_R" style-name)
  
  ;; 6. Restore echo
  (setvar "CMDECHO" old_cmdecho)
)
```

---

## Files Modified

- `autocad-dimstyle/lisp/dimstyle.lsp`
  - Lines 89, 105: DCL label widths changed to 25
  - Lines 813-874: MLEADER style creation rewritten with command-based approach

---

## Commit Details

```
Commit: 44f5a36
Branch: main
Date: 2025-11-05

fix(dimstyle): Fix MLEADER style creation and DCL alignment

- Replace invalid CMLEADERSTYLE setvar with command-based approach
- Use -MLEADERSTYLE command to save and set current style
- Fix DCL layout alignment: scale and dimension distance labels now width 25
- Add CMDECHO control for cleaner command execution
- Resolves setvar rejection error when creating MLEADER styles
```

---

## Next Steps (Optional)

1. **Enhanced MLEADER Features:**
   - Add MLEADER type selection (straight/spline/none)
   - Add arrow type selection (matching dimension arrows)
   - Add landing distance customization

2. **Button Placement:**
   - If user prefers different placement, can restructure DCL
   - Could add tooltips or help text

3. **Error Handling:**
   - Add error checking for command execution
   - Add rollback if style creation fails

4. **Style Synchronization:**
   - Auto-update MLEADER style when dimension style changes
   - Add "Update MLEADER" button to sync styles

---

## References

- AutoCAD MLEADERSTYLE Command: `-MLEADERSTYLE`
- System Variables: CMLEADER* series
- Table Search: `(tblsearch "MLEADERSTYLE" name)`
- Command Execution: `(command "._-MLEADERSTYLE" options...)`
