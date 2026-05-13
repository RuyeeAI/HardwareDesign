```mermaid
%%{init: {'theme': 'base', 'themeVariables': {'fontSize': '12px'}}}%%
flowchart TB

    %% ============================================================
    %% Top-level external inputs (outside subgraphs)
    %% ============================================================
    subgraph ExternalInputs[" "]
        direction TB
        style ExternalInputs fill:none,stroke:none
        L3_ECMP_GROUP_HIER["L3_ECMP_GROUP_HIER"]
        L3_ECMP["L3_ECMP"]
        L3_ECMP_GROUP_COUNT["L3_ECMP_GROUP_COUNT"]
        Lvl2_L3_ECMP["Lvl2_L3_ECMP"]
    end

    subgraph IngressTables["Ingress Tables"]
        direction TB
        style IngressTables fill:none,stroke:none
        ING_L3_NEXT_HOP["ING_L3_NEXT_HOP<br/>32K entries"]
        ING_L3_INITIAL_NEXT_HOP["ING_L3_INITIAL_NEXT_HOP<br/>32K entries"]
    end

    subgraph TrunkTables["Trunk / LAG Tables"]
        direction TB
        style TrunkTables fill:none,stroke:none
        FAST_TRUNK_PORTS["FAST_TRUNK_PORTS<br/>320×64 ports"]
        FAST_TRUNK_SIZE["FAST_TRUNK_SIZE<br/>320×64 TG Size / Trunk Mode"]
    end

    %% ============================================================
    %% Selection logic (stadium nodes)
    %% ============================================================
    NhiSelection["NHI Selection"]
    TpSelection["Egress Port Selection"]
    LAGPortSelection["LAG Port Selection"]
    PathSelection["Path Selection"]
    AggregateMemberAssignment["Aggregate Member Assignment"]
    EvaluteInactivityDuration["Evaluate Inactivity Duration"]
    FLOWSET_INDEX["HASH &amp; FlowSet Indexing"]

    %% ============================================================
    %% DLB subgraph
    %% ============================================================
    subgraph DLB["DLB (Dynamic Load Balancing)"]
        direction TB

        DLB_ID_GEN["Generate DLB_ID"]
        DLB_ECMP_GROUP_CONTROL["DLB_ECMP_GROUP_CONTROL<br/>128 entries"]
        DLB_ECMP_GROUP_MEMBERSHIP["DLB_ECMP_GROUP_MEMBERSHIP<br/>128 entries × 352 × 2"]
        DLB_ECMP_FLOWSET_INST["DLB_ECMP_FLOWSET_INST<br/>32K entries"]
        DLB_ECMP_FLOWSET_MEMBER_INST["DLB_ECMP_FLOWSET_MEMBER_INST<br/>32K entries"]
        DLB_ECMP_GROUP_NHI_MEMBER["DLB_ECMP_GROUP_NHI_MEMBER<br/>128 entries × 64 NHI"]
        DLB_ECMP_GROUP_ALT_NHI_MEMBER["DLB_ECMP_GROUP_ALT_NHI_MEMBER<br/>128 entries × 64 NHI"]
        DLB_ECMP_GROUP_PORT_MEMBER["DLB_ECMP_GROUP_PORT_MEMBER<br/>128 entries × 64 ports"]
        DLB_ECMP_GROUP_ALT_PORT_MEMBER["DLB_ECMP_GROUP_ALT_PORT_MEMBER<br/>128 entries × 64 ports"]
        DLB_PORT_QUALITY_PROFILE["DLB_PORT_QUALITY_PROFILE<br/>64 entries × 352 qualities"]
        DLB_ECMP_GROUP_MONITOR_CONTROL["DLB_ECMP_GROUP_MONITOR_CONTROL<br/>128 entries"]
        DLB_ECMP_GROUP_STATS_INST0["DLB_ECMP_GROUP_STATS_INST0<br/>128 entries"]
    end

    %% ============================================================
    %% Local Quality subgraph
    %% ============================================================
    subgraph LocalQuality["Local Quality Measurement"]
        direction TB

        DLB_ECMP_PORT_AVG_QUALITY_MEASURE["DLB_ECMP_PORT_AVG_QUALITY_MEASURE<br/>QSize / Loading / ITM QSize per Port<br/>352 entries"]
        DLB_ECMP_PORT_AVG_QUALITY_UPDATE_MEASURE_CONTROL["DLB_ECMP_PORT_AVG_QUALITY_UPDATE_MEASURE_CONTROL &amp;<br/>DLB_ECMP_QUANTIZE_CONTROL<br/>QSize / Loading scaling factor, Mapping Ptr<br/>352 entries"]
        DLB_ECMP_QUANTIZED_AVG_QUALITY_MEASURE["DLB_ECMP_QUANTIZED_AVG_QUALITY_MEASURE<br/>Quantized QSize / Loading / ITM QSize per Port<br/>352 entries"]
        DLB_ECMP_PORT_QUALITY_MAPPING["DLB_ECMP_PORT_QUALITY_MAPPING<br/>4096 × 3b"]
        DLB_ECMP_FINAL_MEMBERS_QUALITY_MEASURE["DLB_ECMP_FINAL_MEMBERS_QUALITY_MEASURE<br/>ITM Qualities per Port<br/>352 entries"]
        DLB_ECMP_LINK_CONTROL["DLB_ECMP_LINK_CONTROL"]
        DLB_ECMP_PORT_STATE["DLB_ECMP_PORT_STATE<br/>State / Port"]
        DLB_ECMP_PORT_INST_QUALITY_MEASURE["DLB_ECMP_PORT_INST_QUALITY_MEASURE<br/>Instantaneous ITM0/1 Port QSize and Loading"]
        DLB_ECMP_QUALITY_MEASURE_CONTROL["DLB_ECMP_QUALITY_MEASURE_CONTROL"]
        DLB_ECMP_GLB_QUANTIZE_THRESHOLD["DLB_ECMP_GLB_QUANTIZE_THRESHOLD"]
        FinalPortQuality["FinalPortQuality"]
        AllPathQuality["AllPathQuality"]
    end

    %% ============================================================
    %% Top-level connections (not inside any subgraph)
    %% ============================================================

    %% --- Ingress path ---
    L3_ECMP_GROUP_HIER --> L3_ECMP
    L3_ECMP --> L3_ECMP_GROUP_COUNT
    L3_ECMP_GROUP_COUNT --> Lvl2_L3_ECMP
    Lvl2_L3_ECMP --> NhiSelection
    NhiSelection --> ING_L3_NEXT_HOP
    ING_L3_NEXT_HOP -->|T, TGID| FAST_TRUNK_PORTS
    ING_L3_NEXT_HOP --> FAST_TRUNK_SIZE
    FAST_TRUNK_PORTS -->|Ports| LAGPortSelection
    LAGPortSelection --> TpSelection
    TpSelection --> DLB_ECMP_FLOWSET_INST
    FAST_TRUNK_SIZE -->|TG Size / Mode| LAGPortSelection
    ING_L3_NEXT_HOP --> ING_L3_INITIAL_NEXT_HOP
    ING_L3_INITIAL_NEXT_HOP --> TpSelection

    %% --- DLB ID generation ---
    ECMP_GROUP_DLB_ID_OFFSET["ECMP_GROUP_DLB_ID_OFFSET"]
    L3_ECMP -->|QualityProfile (6b)| DLB_PORT_QUALITY_PROFILE
    L3_ECMP_GROUP_COUNT --> ECMP_GROUP_DLB_ID_OFFSET
    ECMP_GROUP_DLB_ID_OFFSET --> DLB_ID_GEN

    %% --- DLB ID distribution to tables ---
    DLB_ID_GEN -->|DLB_ID| DLB_ECMP_GROUP_CONTROL
    DLB_ID_GEN -->|DLB_ID| DLB_ECMP_GROUP_MEMBERSHIP
    DLB_ID_GEN -->|DLB_ID| DLB_ECMP_GROUP_PORT_MEMBER
    DLB_ID_GEN -->|DLB_ID| DLB_ECMP_GROUP_ALT_PORT_MEMBER
    DLB_ID_GEN -->|DLB_ID| DLB_ECMP_GROUP_NHI_MEMBER
    DLB_ID_GEN -->|DLB_ID| DLB_ECMP_GROUP_ALT_NHI_MEMBER
    DLB_ID_GEN -->|DLB_ID| DLB_ECMP_GROUP_MONITOR_CONTROL

    %% --- DLB control to flowset ---
    DLB_ECMP_GROUP_CONTROL -->|FLOW_BASE, FLOW_OFFSET| FLOWSET_INDEX
    FLOWSET_INDEX --> DLB_ECMP_FLOWSET_INST
    FLOWSET_INDEX --> DLB_ECMP_FLOWSET_MEMBER_INST
    DLB_ECMP_FLOWSET_INST --> EvaluteInactivityDuration
    EvaluteInactivityDuration --> AggregateMemberAssignment
    AggregateMemberAssignment --> DLB_ECMP_FLOWSET_MEMBER_INST
    AggregateMemberAssignment --> DLB_ECMP_GROUP_STATS_INST0

    %% --- Path quality & selection ---
    DLB_ECMP_GROUP_PORT_MEMBER --> PathSelection
    DLB_ECMP_GROUP_NHI_MEMBER --> PathSelection
    DLB_ECMP_GROUP_ALT_PORT_MEMBER --> PathSelection
    DLB_ECMP_GROUP_ALT_NHI_MEMBER --> PathSelection
    PathSelection --> AggregateMemberAssignment
    DLB_PORT_QUALITY_PROFILE -->|remote path quality| AllPathQuality

    %% --- Local quality measurement chain ---
    DLB_ECMP_GLB_QUANTIZE_THRESHOLD --> DLB_ECMP_QUANTIZED_AVG_QUALITY_MEASURE
    DLB_ECMP_PORT_AVG_QUALITY_UPDATE_MEASURE_CONTROL -->|Enable update / Scaling Factor| DLB_ECMP_QUANTIZED_AVG_QUALITY_MEASURE
    DLB_ECMP_PORT_AVG_QUALITY_UPDATE_MEASURE_CONTROL -->|QualityProfile Ptr| DLB_ECMP_PORT_QUALITY_MAPPING
    DLB_ECMP_PORT_AVG_QUALITY_MEASURE --> DLB_ECMP_QUANTIZED_AVG_QUALITY_MEASURE
    DLB_ECMP_QUANTIZED_AVG_QUALITY_MEASURE --> DLB_ECMP_PORT_QUALITY_MAPPING
    DLB_ECMP_PORT_AVG_QUALITY_UPDATE_MEASURE_CONTROL -->|Enable update| DLB_ECMP_FINAL_MEMBERS_QUALITY_MEASURE
    DLB_ECMP_PORT_INST_QUALITY_MEASURE --> DLB_ECMP_FINAL_MEMBERS_QUALITY_MEASURE
    DLB_ECMP_QUALITY_MEASURE_CONTROL --> FinalPortQuality
    DLB_ECMP_PORT_QUALITY_MAPPING --> FinalPortQuality
    DLB_ECMP_FINAL_MEMBERS_QUALITY_MEASURE --> FinalPortQuality
    DLB_ECMP_PORT_STATE --> FinalPortQuality
    DLB_ECMP_LINK_CONTROL --> FinalPortQuality

    %% --- Path quality merging ---
    FinalPortQuality -->|local link quality| AllPathQuality
    AllPathQuality --> PathSelection
```
