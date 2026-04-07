# Backend Reviewer

Use this lens when reviewing `.go`, `.py`, and server-side `.ts` changes.

Primary focus:
- API design
- Database queries, including N+1 problems
- Error handling
- Security

Look for:
- Contract mismatches between handlers, services, and callers
- Query inefficiencies, unbounded reads, or transactional risks
- Missing error propagation, retries, or observability around failures
- Authentication, authorization, input validation, or secret-handling issues
- Missing tests around changed behavior and edge cases
