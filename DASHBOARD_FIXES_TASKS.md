# Dashboard Fixes - CRITICAL TASKS
**Created:** 2026-09-28 23:35
**Status:** URGENT - MUST FIX IMMEDIATELY

## USER REPORTED ISSUES

### CRITICAL Issues
- [ ] **Claude disk space metric MISSING** - restore it
- [ ] **Performance History sorted BACKWARDS** - newest should be FIRST (R40 at top)
- [ ] **Game Browser NOT WORKING** - fix it completely
- [ ] **Public dashboard NOT WORKING** - shows wrong data (R25/R24 instead of R40)
- [ ] **Dashboard audits INCOMPLETE** - not auditing full dashboard
- [ ] **Metrics keep disappearing** - stop removing metrics

### export-static.py Issues
- [x] **Hardcoded to R25/R24** - FIXED: now exports R40, R39, R38, R37, R36
- [ ] **Public site deployment** - need to actually deploy fixed version
- [ ] **Verify public site shows R40 data** - test in browser

### Canonical Store Issues
- [ ] **Winners count = 0** - all 1626 R40 games show won=False (data corruption)
- [ ] **"won" field broken** - investigate and fix

### Dashboard Specification
- [x] **Created DASHBOARD_SPECIFICATION.docx** - defines all required metrics
- [ ] **User review** - waiting for user to review and edit spec
- [ ] **Audit against spec** - once approved, audit all sections

## TICKER PROTOCOL

**NEW RULE:** When dashboard problem found during tick, FIX IT SAME TICK

**Tick workflow:**
1. Check metrics
2. If ANY problem found → STOP
3. Fix the problem IMMEDIATELY
4. Re-run audit
5. Verify fix worked
6. THEN continue tick

## IMPLEMENTATION TASKS

### Fix Performance History Sort
- [ ] Find Performance History table code in server.py
- [ ] Change sort order to newest first (R40, R39, R38...)
- [ ] Test locally
- [ ] Commit to git

### Restore Claude Disk Space
- [ ] Find where disk space was displayed
- [ ] Add it back to dashboard
- [ ] Test locally
- [ ] Commit to git

### Fix Game Browser
- [ ] Identify what's broken (error messages?)
- [ ] Fix the issue
- [ ] Test with R40 data
- [ ] Verify VIEW links work
- [ ] Commit to git

### Fix Canonical Store "won" Field
- [ ] Investigate why all games show won=False
- [ ] Check rebuild_canonical_clean.py logic
- [ ] Fix winner detection for arena games
- [ ] Rebuild canonical store
- [ ] Verify 1347 winners appear correctly

### Deploy Public Dashboard
- [ ] Run fixed export-static.py
- [ ] Upload to oracle server
- [ ] Test public site in browser
- [ ] Verify R40 data shows correctly

## DOCUMENTATION REQUIREMENTS

**ALWAYS:**
- Document every fix in git commit
- Update DASHBOARD_SPECIFICATION.docx if behavior changes
- Test fix locally before deploying
- Re-run full audit after fix
- Mark task complete in this file
