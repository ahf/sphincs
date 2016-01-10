PROJECT = sphincs

PROJECT_DESCRIPTION = SPHINCS: practical stateless hash-based signatures
PROJECT_VERSION = 1.0.0

BLAKE256_IMPL   ?= ref
BLAKE512_IMPL   ?= ref
SPHINCS256_IMPL ?= ref
CHACHA12_IMPL   ?= ref

CRYPTO_CORE_INCLUDE_DIR = $(C_SRC_DIR)/crypto_core/include
CRYPTO_CORE_TEMPLATE_DIR = $(C_SRC_DIR)/crypto_core/templates

CFLAGS  = -Wall -O2 -I$(CRYPTO_CORE_INCLUDE_DIR)
LDFLAGS = -arch x86_64 -flat_namespace -undefined suppress -lsodium -g -ggdb3 

CRYPTO_CORE_SOURCES = $(sort $(foreach pat,*.c,$(call core_find,$(C_SRC_DIR)/crypto_core/src/,$(pat))))
BLAKE256_SOURCES    = $(sort $(foreach pat,*.c,$(call core_find,$(C_SRC_DIR)/crypto_hash/blake256/$(BLAKE256_IMPL)/,$(pat))))
BLAKE512_SOURCES    = $(sort $(foreach pat,*.c,$(call core_find,$(C_SRC_DIR)/crypto_hash/blake512/$(BLAKE512_IMPL)/,$(pat))))
CHACHA12_SOURCES    = $(sort $(foreach pat,*.c,$(call core_find,$(C_SRC_DIR)/crypto_stream/chacha12/$(CHACHA12_IMPL)/,$(pat))))
SPHINCS256_SOURCES  = $(sort $(foreach pat,*.c,$(call core_find,$(C_SRC_DIR)/crypto_sign/sphincs256/$(SPHINCS256_IMPL)/,$(pat))))
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
	$(gen_verbose) sed -e 's/@@@IMPLEMENTATION@@@/$(BLAKE256_IMPL)/g' $< > $@

$(CRYPTO_CORE_INCLUDE_DIR)/crypto_hash_blake512.h:: $(CRYPTO_CORE_TEMPLATE_DIR)/crypto_hash_blake512.h
	$(gen_verbose) sed -e 's/@@@IMPLEMENTATION@@@/$(BLAKE512_IMPL)/g' $< > $@

$(CRYPTO_CORE_INCLUDE_DIR)/crypto_stream_chacha12.h:: $(CRYPTO_CORE_TEMPLATE_DIR)/crypto_stream_chacha12.h
	$(gen_verbose) sed -e 's/@@@IMPLEMENTATION@@@/$(CHACHA12_IMPL)/g' $< > $@
