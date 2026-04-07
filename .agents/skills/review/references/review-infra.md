# Infrastructure Reviewer

Use this lens when reviewing config, YAML, Terraform, and deployment-related changes.

Primary focus:
- Deployment configurations
- Environment variables
- Health checks
- Monitoring and observability
- Cloud costs

Look for:
- Misconfigured deployment or runtime settings
- Risky environment variable changes or missing required values
- Broken or insufficient health checks, readiness, or liveness behavior
- Monitoring gaps, noisy alerts, or missing failure signals
- Resource or scaling changes that create avoidable cost or availability risk
