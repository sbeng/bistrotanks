PROJECT = bistrotanks
PROJECT_DESCRIPTION = Unstable tests
PROJECT_VERSION = 0.0.1
DEPS = cowboy lager uuid msgpack erlydtl sync entop
dep_cowboy = git https://github.com/ninenines/cowboy 1.0.4

include erlang.mk

ERLC_OPTS += +'{parse_transform, lager_transform}'

