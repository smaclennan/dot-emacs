/* ocra interface for XEmacs. */

#include <emodules.h>

#include <openssl/sha.h>
#include <openssl/hmac.h>
#include <openssl/rand.h>

#include <arpa/inet.h>

#define POWER 100000000 /* 8 */

#define OCRA_SUITE "OCRA-1:HOTP-SHA1-8:QN08"
#define QUESTION_LEN 128

static inline uint8_t *sha1(char *secret, uint8_t *key)
{
  EVP_MD_CTX ctx;

  EVP_MD_CTX_init(&ctx);
  if (!EVP_DigestInit_ex(&ctx, EVP_sha1(), NULL) ||
      !EVP_DigestUpdate(&ctx, secret, strlen(secret)) ||
      !EVP_DigestFinal_ex(&ctx, key, NULL))
    exit(1);
  EVP_MD_CTX_cleanup(&ctx);

  return key;
}

/* RFC6287 */
static int generateOCRA(unsigned question, char *secret)
{
  uint8_t msg[sizeof(OCRA_SUITE) + QUESTION_LEN];
  uint8_t key[SHA_DIGEST_LENGTH];
  uint8_t hash[SHA_DIGEST_LENGTH];
  uint8_t *ptr;
  int offset, binary;
  int i, len = sizeof(OCRA_SUITE) + QUESTION_LEN;

  memset(msg, 0, sizeof(msg)); /* zero pad */

  strcpy((char *)msg, OCRA_SUITE);

  /* Find first non-zero nibble */
  for (i = 0; i < 8; ++i)
    if ((question & 0xf0000000) == 0)
      question <<= 4;

  question = htonl(question); /* stupid little endian */
  memcpy(msg + sizeof(OCRA_SUITE), &question, sizeof(question));

  ptr = HMAC(EVP_sha1(), sha1(secret, key), sizeof(key), msg, len, hash, NULL);

  memset(key, 0, sizeof(key));
  memset(msg, 0, len);

  if (!ptr)
    return -1;

  // put selected bytes into result int
  offset = hash[SHA_DIGEST_LENGTH - 1] & 0xf;

  binary =
    ((hash[offset + 0] & 0x7f) << 24) |
    ((hash[offset + 1] & 0xff) << 16) |
    ((hash[offset + 2] & 0xff) << 8) |
    (hash[offset + 3] & 0xff);

  memset(hash, 0, sizeof(hash));

  return binary;
}

DEFUN ("ocra-response", Focra_response, 2, 2, 0, /*
Calculate an ocra response.
QUESTION is number representing the challenge.
SECRET is a string containing the shared secret.
Returns the response as a number.
*/
       (question, secret))
{
  CHECK_INT(question);
  CHECK_STRING(secret);

  return make_int(generateOCRA(XUINT(question), XSTRING_DATA(secret)) % POWER);
}

void
syms_of_ocra (void)
{
  DEFSUBR (Focra_response);
}

void
vars_of_ocra (void)
{
  Fprovide (intern ("ocra"));
}
