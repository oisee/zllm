# ZLLM Architecture Documentation

This document provides detailed architectural diagrams and component documentation for the ZLLM Framework.

## Table of Contents
1. [System Overview](#system-overview)
2. [Component Architecture](#component-architecture)
3. [Data Flow Diagrams](#data-flow-diagrams)
4. [Class Relationships](#class-relationships)
5. [Security Architecture](#security-architecture)
6. [Database Schema](#database-schema)

## System Overview

ZLLM is a multi-layered framework that brings LLM orchestration capabilities to SAP/ABAP systems. The architecture follows a modular design with clear separation of concerns.

### High-Level Architecture

```mermaid
graph TB
    subgraph "External Systems"
        API1[OpenAI API]
        API2[Azure OpenAI]
        API3[Ollama/Local]
    end
    
    subgraph "ZLLM Framework"
        subgraph "Application Layer"
            DEMO[Demo Programs]
            USER[User Applications]
        end
        
        subgraph "Orchestration Layer"
            FLOW[Flow Engine]
            STEP[Step Processor]
            PATTERN[Pattern Engine]
        end
        
        subgraph "Integration Layer"
            CLIENT[LLM Client]
            BALANCE[Load Balancer]
            CACHE[Cache Manager]
        end
        
        subgraph "Infrastructure Layer"
            HTTP[HTTP Handler]
            CODEC[Encryption]
            FILES[File System]
        end
        
        subgraph "Data Layer"
            DB[(Database Tables)]
        end
    end
    
    USER --> FLOW
    DEMO --> FLOW
    FLOW --> STEP
    STEP --> PATTERN
    STEP --> CLIENT
    CLIENT --> BALANCE
    CLIENT --> CACHE
    BALANCE --> HTTP
    HTTP --> API1
    HTTP --> API2
    HTTP --> API3
    CACHE --> DB
    FILES --> CODEC
    CODEC --> DB
```

## Component Architecture

### Core Components Interaction

```mermaid
graph LR
    subgraph "Factory Layer"
        ZCL_LLM[ZCL_LLM<br/>Main Factory]
    end
    
    subgraph "LLM Clients"
        LAZY[ZCL_LLM_00_LLM_LAZY<br/>HTTP Client]
        BALANCER[ZCL_LLM_00_LLM_LAZY_BALANCER<br/>Load Balancer]
        COMPOSITE[ZCL_LLM_00_LLM_LAZY_COMPOSITE<br/>Smart Router]
        MOCK[ZCL_LLM_00_LLM_LAZY_MOCK<br/>Test Mock]
    end
    
    subgraph "Flow Components"
        STEP_LAZY[ZCL_LLM_00_STEP_LAZY<br/>Basic Step]
        STEP_PAR[ZCL_LLM_00_STEP_LAZY_PARALLEL<br/>Parallel Step]
        FLOW_LAZY[ZCL_LLM_00_FLOW_LAZY<br/>Flow Engine]
        FLOW_RES[ZCL_LLM_00_FLOW_RESULT<br/>Result Aggregator]
    end
    
    subgraph "Pattern System"
        PAT[ZCL_LLM_00_PAT<br/>Pattern Engine]
        PAT_LIST[ZCL_LLM_00_PAT_LIST<br/>Pattern Collection]
        FORMULA[ZCL_LLM_00_FORMULA<br/>Formula Builder]
    end
    
    ZCL_LLM --> LAZY
    ZCL_LLM --> BALANCER
    ZCL_LLM --> COMPOSITE
    BALANCER --> LAZY
    COMPOSITE --> LAZY
    ZCL_LLM --> STEP_LAZY
    ZCL_LLM --> FLOW_LAZY
    STEP_LAZY --> PAT
    FLOW_LAZY --> STEP_LAZY
    FLOW_LAZY --> FLOW_RES
    FORMULA --> PAT
```

### Supporting Infrastructure

```mermaid
graph TD
    subgraph "Cache System"
        CACHE_IF[ZIF_LLM_00_CACHE<br/>Interface]
        CACHE_IMPL[ZCL_LLM_00_CACHE<br/>DB Cache]
        CACHE_NEVER[ZCL_LLM_00_CACHE_NEVER<br/>No-Op Cache]
    end
    
    subgraph "File System"
        FILE_IF[ZIF_LLM_00_FILE<br/>Interface]
        FILE_BIN[ZCL_LLM_00_FILE_BIN<br/>Binary Files]
        FILE_LOCAL[ZCL_LLM_00_FILE_LOCAL<br/>Local Files]
        FILE_MOCK[ZCL_LLM_00_FILE_MOCK<br/>Memory Files]
    end
    
    subgraph "Utilities"
        JSON[ZCL_LLM_00_JSON<br/>JSON Handler]
        MARKDOWN[ZCL_LLM_00_MARKDOWN<br/>MD Renderer]
        PREDICTOKEN[ZCL_LLM_00_PREDICTOKEN<br/>Token Predictor]
        DOTENV[ZCL_LLM_00_DOTENV<br/>Config Loader]
    end
    
    subgraph "Security"
        CODEC[ZCL_LLM_00_CODEC<br/>Encryption]
    end
    
    CACHE_IF --> CACHE_IMPL
    CACHE_IF --> CACHE_NEVER
    CACHE_IMPL --> CODEC
    
    FILE_IF --> FILE_BIN
    FILE_IF --> FILE_LOCAL
    FILE_IF --> FILE_MOCK
    FILE_BIN --> CODEC
```

## Data Flow Diagrams

### Request Processing Flow

```mermaid
sequenceDiagram
    participant App as Application
    participant Factory as ZCL_LLM
    participant Step as Step Component
    participant Pattern as Pattern Engine
    participant Client as LLM Client
    participant Cache as Cache System
    participant HTTP as HTTP Handler
    participant API as LLM API
    
    App->>Factory: Create LLM instance
    Factory->>Client: Initialize with config
    Client->>Cache: Initialize cache
    
    App->>Step: Create step with prompt
    Step->>Pattern: Parse template
    
    App->>Step: Execute step
    Step->>Pattern: Apply data to template
    Pattern-->>Step: Formatted prompt
    
    Step->>Client: Send request
    Client->>Cache: Check cache
    
    alt Cache Hit
        Cache-->>Client: Return cached response
    else Cache Miss
        Client->>HTTP: Send HTTP request
        HTTP->>API: POST /chat/completions
        API-->>HTTP: Response
        HTTP-->>Client: Parse response
        Client->>Cache: Store in cache
    end
    
    Client-->>Step: Return result
    Step-->>App: Final result
```

### Flow Orchestration

```mermaid
sequenceDiagram
    participant App as Application
    participant Flow as Flow Engine
    participant Step1 as Step 1
    participant Step2 as Step 2
    participant Step3 as Step 3
    participant Result as Flow Result
    
    App->>Flow: Create flow with steps
    App->>Flow: Execute flow
    
    Flow->>Step1: Execute with input
    Step1-->>Flow: Result 1
    
    Flow->>Step2: Execute with Result 1
    Step2-->>Flow: Result 2
    
    Flow->>Step3: Execute with Result 2
    Step3-->>Flow: Result 3
    
    Flow->>Result: Aggregate results
    Result-->>Flow: Combined output
    Flow-->>App: Final result
```

## Class Relationships

### Interface Hierarchy

```mermaid
classDiagram
    class ZIF_LLM_00_LLM_LAZY {
        <<interface>>
        +q(io_json) llm_response
        +a(io_response) string
        +get_config() config
    }
    
    class ZIF_LLM_00_STEP_LAZY {
        <<interface>>
        +start(ir_data) step_result
        +exec(ir_data) data
        +collect(io_result) data
    }
    
    class ZIF_LLM_00_PAT {
        <<interface>>
        +apply(ir_data) string
        +get_name() string
    }
    
    class ZIF_LLM_00_CACHE {
        <<interface>>
        +get(key) string
        +put(key, value)
        +invalidate(key)
        +clear()
    }
    
    class ZIF_LLM_00_FILE {
        <<interface>>
        +get_string() string
        +get_xstring() xstring
        +get_name() string
    }
    
    ZCL_LLM_00_LLM_LAZY ..|> ZIF_LLM_00_LLM_LAZY
    ZCL_LLM_00_LLM_LAZY_BALANCER ..|> ZIF_LLM_00_LLM_LAZY
    ZCL_LLM_00_LLM_LAZY_COMPOSITE ..|> ZIF_LLM_00_LLM_LAZY
    ZCL_LLM_00_LLM_LAZY_MOCK ..|> ZIF_LLM_00_LLM_LAZY
    
    ZCL_LLM_00_STEP_LAZY ..|> ZIF_LLM_00_STEP_LAZY
    ZCL_LLM_00_STEP_LAZY_PARALLEL ..|> ZIF_LLM_00_STEP_LAZY
    
    ZCL_LLM_00_PAT ..|> ZIF_LLM_00_PAT
    
    ZCL_LLM_00_CACHE ..|> ZIF_LLM_00_CACHE
    ZCL_LLM_00_CACHE_NEVER ..|> ZIF_LLM_00_CACHE
    
    ZCL_LLM_00_FILE_BIN ..|> ZIF_LLM_00_FILE
    ZCL_LLM_00_FILE_LOCAL ..|> ZIF_LLM_00_FILE
    ZCL_LLM_00_FILE_MOCK ..|> ZIF_LLM_00_FILE
```

### Component Dependencies

```mermaid
graph TD
    subgraph "Core Dependencies"
        ZCL_LLM --> ZCL_LLM_00_CACHE
        ZCL_LLM --> ZCL_LLM_00_CODEC
        ZCL_LLM --> ZCL_LLM_00_FILE_LIST_BIN
        ZCL_LLM --> ZCL_LLM_00_LLM_LAZY
        
        ZCL_LLM_00_LLM_LAZY --> ZCL_LLM_00_LLM_RESPONSE
        ZCL_LLM_00_LLM_LAZY --> ZCL_LLM_00_CACHE
        ZCL_LLM_00_LLM_LAZY --> ZCL_LLM_00_PREDICTOKEN
        
        ZCL_LLM_00_STEP_LAZY --> ZCL_LLM_00_PAT
        ZCL_LLM_00_STEP_LAZY --> ZCL_LLM_00_STEP_RESULT
        
        ZCL_LLM_00_FLOW_LAZY --> ZCL_LLM_00_STEP_LAZY
        ZCL_LLM_00_FLOW_LAZY --> ZCL_LLM_00_FLOW_RESULT
    end
```

## Security Architecture

### Encryption and Storage Flow

```mermaid
graph LR
    subgraph "User Layer"
        CRED[API Credentials]
        CONFIG[Configuration]
        DATA[User Data]
    end
    
    subgraph "Encryption Layer"
        CODEC[ZCL_LLM_00_CODEC<br/>XOR Encryption]
        SEED[User Seed<br/>ZLLM_CODEC]
    end
    
    subgraph "Storage Layer"
        CACHE_ENC[Encrypted Cache]
        BIN_ENC[Encrypted Files]
    end
    
    subgraph "Database"
        CACHE_TAB[(ZLLM_00_CACHE)]
        BIN_TAB[(ZLLM_00_BIN)]
    end
    
    CRED --> CODEC
    CONFIG --> CODEC
    DATA --> CODEC
    SEED --> CODEC
    
    CODEC --> CACHE_ENC
    CODEC --> BIN_ENC
    
    CACHE_ENC --> CACHE_TAB
    BIN_ENC --> BIN_TAB
```

### Security Features

```mermaid
graph TD
    subgraph "Security Layers"
        A[Automatic Encryption<br/>Enabled by Default]
        B[User Isolation<br/>Seed-based Separation]
        C[API Key Protection<br/>Never Plain Text]
        D[Configurable Seeds<br/>ZLLM_CODEC Parameter]
    end
    
    subgraph "Implementation"
        E[ZCL_LLM_00_CODEC<br/>Symmetric XOR]
        F[ZCL_LLM_00_CACHE<br/>Encrypted Storage]
        G[ZCL_LLM_00_FILE_BIN<br/>Encrypted Files]
    end
    
    A --> E
    B --> E
    C --> F
    C --> G
    D --> E
```

## Database Schema

### Table Relationships

```mermaid
erDiagram
    ZLLM_00_NODE {
        string MANDT
        int SEED
        string NODE
        date CDATE
        string OBJ_TYPE
        string OBJ_NAME
        string DEVCLASS
        string INCLUDE
    }
    
    ZLLM_00_EDGE {
        string MANDT
        string ETYPE
        int SEED
        date CDATE
        timestamp TS
    }
    
    ZLLM_00_CACHE {
        string MANDT
        int SEED
        date CDATE
        date ADATE
        int ACCESSED
        timestamp TS
        blob VALUE
    }
    
    ZLLM_00_BIN {
        string BIN
        string MANDT
        string NAME
        date CDATE
        timestamp TS
        blob CONTENT
    }
    
    ZLLM_00_DOC {
        string NODE
        string MANDT
        date CDATE
        timestamp TS
        text DOCUMENTATION
    }
    
    ZLLM_00_CCLM {
        string MANDT
        int SEED
        string OBJ_TYPE
        string OBJ_NAME
        string NODE
        date LAST_USED
        int AGE_Y
        int AGE_M
        int AGE_D
    }
    
    ZLLM_00_NODE ||--o{ ZLLM_00_EDGE : "relates to"
    ZLLM_00_NODE ||--o| ZLLM_00_DOC : "has documentation"
    ZLLM_00_NODE ||--o| ZLLM_00_CCLM : "lifecycle tracking"
```

### Data Storage Architecture

```mermaid
graph TD
    subgraph "Transient Data"
        CACHE[Cache Entries<br/>ZLLM_00_CACHE<br/>~750 rows]
    end
    
    subgraph "Code Analysis"
        NODE[Code Objects<br/>ZLLM_00_NODE<br/>~15,000 rows]
        EDGE[Relationships<br/>ZLLM_00_EDGE<br/>~500 rows]
    end
    
    subgraph "Binary Storage"
        BIN[Files & Configs<br/>ZLLM_00_BIN<br/>~250 rows]
    end
    
    subgraph "Metadata"
        DOC[Documentation<br/>ZLLM_00_DOC]
        CCLM[Lifecycle<br/>ZLLM_00_CCLM]
    end
    
    style CACHE fill:#f9f,stroke:#333,stroke-width:2px
    style NODE fill:#9ff,stroke:#333,stroke-width:2px
    style EDGE fill:#9ff,stroke:#333,stroke-width:2px
    style BIN fill:#ff9,stroke:#333,stroke-width:2px
```

## Performance Considerations

### Caching Strategy

```mermaid
graph LR
    subgraph "Request Flow"
        REQ[LLM Request] --> HASH[Generate Hash]
        HASH --> CHECK{Cache Check}
        CHECK -->|Hit| CACHED[Return Cached]
        CHECK -->|Miss| API[Call API]
        API --> STORE[Store in Cache]
        STORE --> RETURN[Return Response]
    end
    
    subgraph "Cache Management"
        CACHE[(ZLLM_00_CACHE)]
        TRIM[Trim Old Entries<br/>14 days]
        CLEAR[Clear Cache<br/>By Seed]
    end
    
    STORE --> CACHE
    CACHE --> TRIM
    CACHE --> CLEAR
```

### Token Prediction Flow

```mermaid
graph TD
    INPUT[Input Text] --> FEATURES[Extract Features<br/>• Length<br/>• Words<br/>• Punctuation<br/>• etc.]
    FEATURES --> MODEL{Model Type}
    MODEL -->|GPT-4| GPT[GPT Coefficients]
    MODEL -->|Mistral| MISTRAL[Mistral Coefficients]
    GPT --> CALC[Calculate Tokens<br/>Linear Regression]
    MISTRAL --> CALC
    CALC --> RESULT[Predicted Tokens<br/>99.7% Accuracy]
    
    style RESULT fill:#9f9,stroke:#333,stroke-width:2px
```

## Deployment Architecture

### Package Structure

```mermaid
graph TD
    subgraph "$ZLLM_00 Package"
        subgraph "Core Framework"
            ZCL_LLM[Factory & Core]
            LLM_CLASSES[LLM Client Classes]
        end
        
        subgraph "Flow Engine"
            STEP_CLASSES[Step Components]
            FLOW_CLASSES[Flow Components]
        end
        
        subgraph "Infrastructure"
            CACHE_CLASSES[Cache System]
            FILE_CLASSES[File System]
            UTIL_CLASSES[Utilities]
        end
        
        subgraph "Demo Programs"
            ONBOARD[ZLLM_00_ONBOARD]
            FLOW_DEMO[ZLLM_00_FLOW_DEMO]
            REPL[ZLLM_00_REPL]
            SYNC[ZLLM_00_SYNC]
        end
        
        subgraph "Database"
            TABLES[(Custom Tables)]
        end
    end
```

## Summary

The ZLLM Framework architecture is designed with:

1. **Modularity**: Clear separation between layers and components
2. **Security**: Built-in encryption and user isolation
3. **Performance**: Smart caching and token prediction
4. **Flexibility**: Multiple LLM provider support
5. **Extensibility**: Interface-based design for easy extension
6. **Enterprise-Ready**: Robust error handling and monitoring

This architecture enables developers to build sophisticated LLM-powered applications within SAP/ABAP environments while maintaining security, performance, and maintainability standards.