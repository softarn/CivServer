
#ifndef FAILDRESULT_H
#define FAILDRESULT_H

#include <string>
#include "result.h"


class FaildResult : public Result
{
public:
	static const char *TYPE;

	enum FAILURES
	{
		INVALID_STATE = -1,
		INVALID_PROTOCOL_VERSION = 0
	};

private:
	int m_failureNumber;
	int m_failedHeader;
	std::string m_description;

public:
	FaildResult(int failureNumber, int failedHeader, const std::string &description);

	int getFailureNumber() const;
	int getFailedHeader() const;
	const std::string &getDescription() const;


	const char *getType() const;
};

#endif // FAILDRESULT_H
