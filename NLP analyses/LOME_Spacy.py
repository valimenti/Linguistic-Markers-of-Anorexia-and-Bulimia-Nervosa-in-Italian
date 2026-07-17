
import json
import spacy
import os
import pandas as pd
from collections import defaultdict


# Load spaCy model
nlp = spacy.load("it_core_news_lg")

# Directory containing JSON files
input_directory = "/Users/.../json_output_lome_bn"



rows = []

# =========================
# Allign tokens by character span
# =========================
def align_tokens_by_char_span(lome_data, spacy_doc):
    alignment_map = {}

    for mention in lome_data['situationMentionSetList'][0]['mentionList']:
        lome_text = mention['text']
        start_idx = lome_data['text'].find(lome_text)
        end_idx = start_idx + len(lome_text)

        spacy_tokens = [
            token for token in spacy_doc
            if start_idx <= token.idx < end_idx
        ]

        alignment_map[mention["uuid"]["uuidString"]] = spacy_tokens

    return alignment_map


# =========================
# Get root token
# =========================
def get_root_token(tokens):
    token_set = set(tokens)
    for token in tokens:
        if token.head not in token_set:
            return token
    return tokens[0] if tokens else None


# =========================
# Classification of token based on its role and syntactic properties
# =========================
def classify_token(token, roles, situation_kind, children):

    # 1. NON VERB
    if token.pos_ != "VERB":
        if token.pos_ in ["ADJ", "NOUN"] and any(child.dep_ == "cop" for child in children):
            return "nominal"
        return "arg"

    # 2. PASSIVE 
    if any(child.dep_ in ["nsubj:pass", "aux:pass"] for child in children):
        if any(child.dep_ == "obl:agent" for child in children):
            return "passive_with_agent"
        return "passive"

    # 3. IMPERSONAL
    if any(child.dep_ == "expl:impers" for child in children) or (
        "Precipitation" in roles and not any(child.dep_ == "nsubj" for child in children)
    ):
        return "impersonal"

    # 4. ACTIVE
    if any(child.dep_ == "obj" for child in children):
        return "active"

    if any(child.lemma_ == "avere" and child.pos_ == "AUX" for child in children):
        return "active"

    # 5. ANTICAUSATIVE
    if any(child.lemma_ == "essere" and child.pos_ == "AUX" for child in children):
        return "anticausative"

    if (
        not any(child.dep_ == "obj" for child in children)
        and any(role in roles for role in ["Theme", "Entity", "Patient"])
    ):
        return "anticausative"


    # 6. DEFAULT neutro
    return "active"



# =========================
# Process files in the input directory
# =========================
for filename in sorted(os.listdir(input_directory)):

    if not filename.endswith(".json"):
        continue

    filepath = os.path.join(input_directory, filename)

    with open(filepath, encoding="utf-8") as f:
        lome_data = json.load(f)

    doc = nlp(lome_data["text"])
    alignment_map = align_tokens_by_char_span(lome_data, doc)

    file_stats = defaultdict(lambda: {
        "total": 0,
        "passive": 0,
        "passive_with_agent": 0,
        "active": 0,
        "anticausative": 0,
        "arg": 0,
        "impersonal_nominal": 0,
        "triggers": []
    })

    for mention in lome_data['situationMentionSetList'][0]['mentionList']:

        frame_label = mention["situationKind"]
        mention_uuid = mention["uuid"]["uuidString"]

        spacy_tokens = alignment_map.get(mention_uuid, [])
        if not spacy_tokens:
            continue

        root_token = get_root_token(spacy_tokens)
        if not root_token:
            continue

        children = list(root_token.children)
        roles = mention.get("roles", [])

        construction_type = classify_token(
            root_token,
            roles,
            frame_label,
            children
        )

        stats = file_stats[frame_label]

        stats["total"] += 1
        stats["triggers"].append(root_token.text)

        if construction_type == "passive":
            stats["passive"] += 1
        elif construction_type == "passive_with_agent":
            stats["passive_with_agent"] += 1
        elif construction_type == "active":
            stats["active"] += 1
        elif construction_type == "anticausative":
            stats["anticausative"] += 1
        elif construction_type == "arg":
            stats["arg"] += 1
        elif construction_type in ["impersonal", "nominal"]:
            stats["impersonal_nominal"] += 1

    # create output file
    for frame, stats in file_stats.items():
        rows.append({
            "File": filename,
            "Frame": frame,
            "Totale_occorrenze_frame": stats["total"],
            "Costruzioni_passive": stats["passive"],
            "Passive_con_agente": stats["passive_with_agent"],
            "Costruzioni_attive": stats["active"],
            "Costruzioni_anticausative": stats["anticausative"],
            "Costruzioni_con_argomento": stats["arg"],
            "Impersonali_e_nominali": stats["impersonal_nominal"],
            "Frame_triggers": ", ".join(stats["triggers"])
        })



df = pd.DataFrame(rows)
df
# df.to_excel("output.xlsx", index=False)


