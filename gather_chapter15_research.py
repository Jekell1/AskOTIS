#!/usr/bin/env python3
"""
Chapter 15 Research: Deployment and Rollout
Gathers answers to 50 questions about DevOps practices, staging/production deployment,
rollback strategies, and training/documentation for the modernized system.
"""

import json
import time
from otis_rag import OTISRAG

def main():
    rag = OTISRAG()
    
    questions = [
        # Section 15.1: DevOps for Modernized Systems (Q1-17)
        "What DevOps practices are essential for deploying and maintaining C# APIs in production?",
        "How should we set up CI/CD pipelines for automated building, testing, and deployment?",
        "What tools (Azure DevOps, GitHub Actions, Jenkins) are best suited for .NET API deployments?",
        "How can we implement Infrastructure as Code (IaC) using tools like Terraform or ARM templates?",
        "What containerization strategies (Docker) benefit C# API deployments?",
        "How should we orchestrate containerized APIs using Kubernetes or Azure Container Apps?",
        "What monitoring and observability tools (Application Insights, Prometheus) should be in place?",
        "How can we implement automated health checks and self-healing mechanisms?",
        "What secrets management strategies (Azure Key Vault, HashiCorp Vault) protect sensitive configuration?",
        "How should we version APIs and manage breaking vs. non-breaking changes?",
        "What blue-green or canary deployment strategies minimize risk during rollout?",
        "How can we implement feature flags to control new functionality rollout?",
        "What disaster recovery and backup strategies should be in place for the new system?",
        "How should we handle database schema migrations in a DevOps pipeline?",
        "What strategies exist for automated rollback if a deployment fails?",
        "How can we implement progressive delivery to gradually expose new features to users?",
        "What security scanning (SAST, DAST) should be integrated into the CI/CD pipeline?",
        
        # Section 15.2: Staging, Production, and Rollback Plans (Q18-34)
        "How should we structure staging environments to mirror production accurately?",
        "What testing should occur in staging before promoting to production?",
        "How can we implement smoke tests to validate deployments in each environment?",
        "What strategies exist for zero-downtime deployments to production?",
        "How should we coordinate database migrations with application deployments?",
        "What rollback procedures should be in place if production issues arise?",
        "How can we implement automated rollback based on error rates or health checks?",
        "What communication plans notify stakeholders during deployment windows?",
        "How should we handle configuration differences between staging and production?",
        "What approval gates or manual checks should exist before production deployment?",
        "How can we implement gradual rollout to production (e.g., deploy to subset of servers first)?",
        "What strategies exist for rolling back database migrations if needed?",
        "How should we test rollback procedures to ensure they work when needed?",
        "What post-deployment validation confirms the system is functioning correctly?",
        "How can we coordinate deployment with business operations to minimize disruption?",
        "What runbooks or playbooks guide operators through deployment and rollback scenarios?",
        "How should we handle legacy system cutover timing and coordination?",
        
        # Section 15.3: Training and Documentation (Q35-50)
        "What training materials should be created for end-users of the new front end?",
        "How should we train IT staff on operating and supporting the new C# API architecture?",
        "What documentation is essential for developers maintaining the modernized codebase?",
        "How can we create API documentation using tools like Swagger/OpenAPI?",
        "What operational runbooks guide troubleshooting and incident response?",
        "How should we document architectural decisions and migration rationale?",
        "What knowledge transfer processes ensure legacy COBOL expertise is not lost?",
        "How can we create video tutorials or interactive training for the new system?",
        "What role does hands-on training in sandbox environments play in user adoption?",
        "How should we document known issues, workarounds, and limitations of the new system?",
        "What strategies exist for ongoing training as the system evolves?",
        "How can we gather user feedback post-launch to improve training and documentation?",
        "What support structures (help desk, chatbots) should be in place post-deployment?",
        "How should we document business processes that changed due to modernization?",
        "What strategies exist for maintaining documentation over time as the system evolves?",
        "How can we create a knowledge base or wiki for common questions and troubleshooting?",
    ]
    
    results = []
    
    print(f"\n{'='*60}")
    print(f"CHAPTER 15 RESEARCH: Deployment and Rollout")
    print(f"{'='*60}")
    print(f"Total questions to process: {len(questions)}\n")
    
    for i, question in enumerate(questions, 1):
        print(f"\n[{i}/{len(questions)}] Researching: {question[:80]}...")
        
        start_time = time.time()
        response = rag.ask_with_timing(question)
        elapsed = time.time() - start_time
        
        result = {
            "question_number": i,
            "question": question,
            "answer": response['answer'],
            "sources_count": response['timing'].get('num_documents', 0),
            "query_time_seconds": response['timing']
        }
        results.append(result)
        
        # Save incrementally
        with open("chapter15_deployment_rollout_research.json", "w", encoding="utf-8") as f:
            json.dump(results, f, indent=2, ensure_ascii=False)
        
        print(f"✓ Answer received ({result['sources_count']} sources, {elapsed:.2f}s)")
    
    print(f"\n{'='*60}")
    print(f"✅ CHAPTER 15 RESEARCH COMPLETE!")
    print(f"{'='*60}")
    print(f"Results saved to: chapter15_deployment_rollout_research.json")
    print(f"Total questions answered: {len(results)}")

if __name__ == "__main__":
    main()
