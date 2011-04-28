
#include "confirmdresult.h"


const char *ConfirmdResult::TYPE = "Confirm'd result";


ConfirmdResult::ConfirmdResult(int confirmedHeader)
	: m_confirmedHeader(confirmedHeader)
{
}

int ConfirmdResult::getConfirmedHeader() const
{
	return m_confirmedHeader;
}

const char *ConfirmdResult::getType() const
{
	return TYPE;
}
