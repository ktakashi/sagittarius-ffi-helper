#ifndef INC_MACROS
#define INC_MACROS

/* copied from sagittariusdef.h */
#if defined(__CYGWIN__) || defined(SAGITTARIUS_WINDOWS)
# if defined(LIBSAGITTARIUS_BODY)
#  define SG_EXPORT __declspec(dllexport)
# else
#  define SG_EXPORT __declspec(dllimport)
# endif
# define SG_EXTERN extern SG_EXPORT
#else
# define SG_EXPORT 
# define SG_EXTERN extern
#endif

#ifdef __cplusplus
# define __STDC_LIMIT_MACROS
# define SG_CDECL_BEGIN extern "C" {
# define SG_CDECL_END }
#else
# define SG_CDECL_BEGIN
# define SG_CDECL_END
#endif

/* copied from sagittarius-ffi.h and modified to #define */
#define FFI_RETURN_TYPE_VOID      0x0000
#define FFI_RETURN_TYPE_BOOL      0x0001
#define FFI_RETURN_TYPE_SHORT     0x0002
#define FFI_RETURN_TYPE_INT       0x0003
#define FFI_RETURN_TYPE_LONG      0x0004
#define FFI_RETURN_TYPE_INTPTR    0x0005
#define FFI_RETURN_TYPE_USHORT    0x0006
#define FFI_RETURN_TYPE_UINT      0x0007
#define FFI_RETURN_TYPE_ULONG     0x0008
#define FFI_RETURN_TYPE_UINTPTR   0x0009
#define FFI_RETURN_TYPE_FLOAT     0x000a
#define FFI_RETURN_TYPE_DOUBLE    0x000b
#define FFI_RETURN_TYPE_STRING    0x000c
#define FFI_RETURN_TYPE_SIZE_T    0x000d
#define FFI_RETURN_TYPE_INT8_T    0x000e
#define FFI_RETURN_TYPE_UINT8_T   0x000f
#define FFI_RETURN_TYPE_INT16_T   0x0010
#define FFI_RETURN_TYPE_UINT16_T  0x0011
#define FFI_RETURN_TYPE_INT32_T   0x0012
#define FFI_RETURN_TYPE_UINT32_T  0x0013
#define FFI_RETURN_TYPE_INT64_T   0x0014
#define FFI_RETURN_TYPE_UINT64_T  0x0015
#define FFI_RETURN_TYPE_POINTER   0x0016
#define FFI_RETURN_TYPE_STRUCT    0x0017
#define FFI_RETURN_TYPE_CALLBACK  0x0018
#define FFI_RETURN_TYPE_WCHAR_STR 0x0019
#define FFI_RETURN_TYPE_LAST      FFI_RETURN_TYPE_WCHAR_STR

#define NUM_RET_TYPE FFI_RETURN_TYPE_LAST+1


#endif	/* INC_MACROS */
