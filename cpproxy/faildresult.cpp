
#include "faildresult.h"

const char *FaildResult::TYPE = "Fail'd result";


FaildResult::FaildResult(int failureNumber, int failedHeader, const std::string &description)
	: m_failureNumber(failureNumber),
	  m_failedHeader(failedHeader),
	  m_description(description)
{
}

int FaildResult::getFailureNumber() const
{
	return m_failureNumber;
}

int FaildResult::getFailedHeader() const
{
	return m_failedHeader;
}

const std::string &FaildResult::getDescription() const
{
	return m_description;
}


const char *FaildResult::getType() const
{
	return TYPE;
}
