PROJECT = sphincs

PROJECT_DESCRIPTION = SPHINCS: practical stateless hash-based signatures
PROJECT_VERSION = 1.0.0

LOCAL_DEPS = crypto

BLAKE256   ?= ref
BLAKE512   ?= ref
SPHINCS256 ?= ref
CHACHA12   ?= ref

CRYPTO_CORE_INCLUDE_DIR = $(C_SRC_DIR)/crypto_core/include
CRYPTO_CORE_TEMPLATE_DIR = $(C_SRC_DIR)/crypto_core/templates

CFLAGS  = -Wall -O2 -I$(CRYPTO_CORE_INCLUDE_DIR)

CRYPTO_CORE_SOURCES = $(sort $(foreach pat,*.c,$(call core_find,$(C_SRC_DIR)/crypto_core/src/,$(pat))))
BLAKE256_SOURCES    = $(sort $(foreach pat,*.c,$(call core_find,$(C_SRC_DIR)/crypto_hash/blake256/$(BLAKE256)/,$(pat))))
BLAKE512_SOURCES    = $(sort $(foreach pat,*.c,$(call core_find,$(C_SRC_DIR)/crypto_hash/blake512/$(BLAKE512)/,$(pat))))
CHACHA12_SOURCES    = $(sort $(foreach pat,*.c,$(call core_find,$(C_SRC_DIR)/crypto_stream/chacha12/$(CHACHA12)/,$(pat))))
SPHINCS256_SOURCES  = $(sort $(foreach pat,*.c *.s,$(call core_find,$(C_SRC_DIR)/crypto_sign/sphincs256/$(SPHINCS256)/,$(pat))))
ERLANG_NIF_SOURCES  = $(sort $(foreach pat,*.c,$(call core_find,$(C_SRC_DIR)/erlang_nif/,$(pat))))

SOURCES = $(CRYPTO_CORE_SOURCES) $(BLAKE256_SOURCES) $(BLAKE512_SOURCES) $(CHACHA12_SOURCES) $(SPHINCS256_SOURCES) $(ERLANG_NIF_SOURCES)

TEST_DEPS = triq

dep_triq = git https://github.com/krestenkrab/triq master

include erlang.mk

CRYPTO_CORE_HEADERS = \
	$(CRYPTO_CORE_INCLUDE_DIR)/crypto_hash_blake256.h \
	$(CRYPTO_CORE_INCLUDE_DIR)/crypto_hash_blake512.h \
	$(CRYPTO_CORE_INCLUDE_DIR)/crypto_stream_chacha12.h

$(PROJECT).d:: $(CRYPTO_CORE_HEADERS)
	echo $(CHACHA12_SOURCES)

$(CRYPTO_CORE_INCLUDE_DIR)/crypto_hash_blake256.h:: $(CRYPTO_CORE_TEMPLATE_DIR)/crypto_hash_blake256.h
	$(gen_verbose) sed -e 's/@@@IMPLEMENTATION@@@/$(BLAKE256)/g' $< > $@

$(CRYPTO_CORE_INCLUDE_DIR)/crypto_hash_blake512.h:: $(CRYPTO_CORE_TEMPLATE_DIR)/crypto_hash_blake512.h
	$(gen_verbose) sed -e 's/@@@IMPLEMENTATION@@@/$(BLAKE512)/g' $< > $@

$(CRYPTO_CORE_INCLUDE_DIR)/crypto_stream_chacha12.h:: $(CRYPTO_CORE_TEMPLATE_DIR)/crypto_stream_chacha12.h
	$(gen_verbose) sed -e 's/@@@IMPLEMENTATION@@@/$(CHACHA12)/g' $< > $@
