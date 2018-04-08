#ifndef RRNR_LOCAL_H
#define RRNR_LOCAL_H

#define SHOULD_BE_HANDLED    0x1
#define CAN_PRODUCE_CALLBACK 0x2
#define IS_ENV_GENERATOR     0x4
#define IS_DEVICE_OPENING    0x8
#define IS_SINK              0x10
#define IS_WRITE             0x20
#define IS_INTERNAL          0x40

typedef struct
{
	int allow_graphs;
	int allow_prints;
	int allow_connections;
} RRnR_Options;

extern RRnR_Options RRnR_options;

int detect_flags(SEXP call, SEXP op, SEXP args, SEXP env, int call_type);

#endif
