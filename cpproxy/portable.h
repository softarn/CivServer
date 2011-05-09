
#ifndef PORTABLE_H
#define PORTABLE_H


#ifdef _MSC_VER
	typedef unsigned __int8 uint8_t;
	typedef __int8 int8_t;

	typedef unsigned __int32 uint32_t;
	typedef __int32 int32_t;
#else
	#include <stdint.h>
#endif



#endif // PORTABLE_H